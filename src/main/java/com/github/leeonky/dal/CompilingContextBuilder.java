package com.github.leeonky.dal;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.*;
import com.github.leeonky.dal.token.IllegalTypeException;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.SubTypeViaString;
import com.github.leeonky.dal.util.*;

import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CompilingContextBuilder {
    private final TypeData<PropertyAccessor> propertyAccessors = new TypeData<>();
    private final Map<String, Function<Object, Object>> typeDefinitions = new LinkedHashMap<>();
    private final TypeData<ListAccessor> listAccessors = new TypeData<>();
    private final Set<Class<?>> schemas = new HashSet<>();

    public CompilingContextBuilder() {
        registerValueFormat(new PositiveInteger());
        registerValueFormat(new URL());
        registerValueFormat(new Instant());
        registerValueFormat(new FormatterString());
    }

    public static <T> T requiredType(boolean rightType, Supplier<T> supplier) {
        if (rightType)
            return supplier.get();
        throw new IllegalTypeException();
    }

    private static Optional<Type> getGenericParams(Type type, int index) {
        return Optional.of(type)
                .filter(ParameterizedType.class::isInstance)
                .map(ParameterizedType.class::cast)
                .map(p -> p.getActualTypeArguments()[index]);
    }

    private static Class<?> guessType(String subPrefix, Type elementType) {
        Class<?> subType;
        if (elementType instanceof Class<?>)
            subType = (Class<?>) elementType;
        else if (elementType instanceof ParameterizedType)
            subType = (Class<?>) ((ParameterizedType) elementType).getRawType();
        else
            throw new IllegalArgumentException("Can not process " + subPrefix + " in type " + elementType);
        return subType;
    }

    public CompilingContextBuilder registerValueFormat(Formatter formatter) {
        return registerValueFormat(formatter.getFormatterName(), formatter);
    }

    @SuppressWarnings("unchecked")
    public CompilingContextBuilder registerValueFormat(String name, Formatter formatter) {
        typeDefinitions.put(name, o -> {
            if (formatter.isValidType(o))
                return formatter.toValue(o);
            throw new IllegalTypeException();
        });
        return this;
    }

    public CompilingContext build(Object inputValue) {
        return new CompilingContext(inputValue, propertyAccessors, typeDefinitions, listAccessors);
    }

    public CompilingContextBuilder registerSchema(Class<?> clazz) {
        return registerSchema(clazz.getSimpleName(), clazz);
    }

    public CompilingContextBuilder registerSchema(String name, Class<?> clazz) {
        schemas.add(clazz);
        return registerSchema(name, bw -> isRightObject(clazz, bw, ""));
    }

    private boolean isRightObject(Class<?> clazz, WrappedObject bw, String subPrefix) {
        Set<Member> members = BeanUtil.findPropertyReaders(clazz);
        Set<String> expectedFields = members.stream().map(Member::getName).collect(Collectors.toSet());
        Set<String> actualFields = bw.getPropertyReaderNames();
        for (String f : actualFields) {
            if (!expectedFields.contains(f)) {
                System.err.printf("Unexpected field `%s` for type %s[%s]\n", f, clazz.getSimpleName(), clazz.getName());
                return false;
            }
        }
        for (Member member : members) {
            Field field = (Field) member;
            boolean allowNull = field.getAnnotation(AllowNull.class) != null;
            if (!allowNull && !actualFields.contains(field.getName())) {
                System.err.printf("Expected field `%s` for type %s[%s], but does not exist\n", field.getName(), clazz.getSimpleName(), clazz.getName());
                return false;
            }
            WrappedObject propertyValueWrapper = bw.getPropertyValueWrapper(member.getName());
            if (allowNull && propertyValueWrapper.isNull())
                continue;

            Class<?> fieldType = field.getType();
            if (!isRightObject(subPrefix + "." + field.getName(), propertyValueWrapper, fieldType, field.getGenericType()))
                return false;
        }
        return true;
    }

    @SuppressWarnings("unchecked")
    private boolean isRightObject(String subPrefix, WrappedObject wrapperValue, Class<?> fieldType, Type genericType) {
        if (Formatter.class.isAssignableFrom(fieldType)) {
            try {
                Formatter formatter = (Formatter) fieldType.getConstructor().newInstance();
                if (!formatter.isValidValue(wrapperValue.getValue())) {
                    System.err.printf("Expected field `%s` shoub be in %s, but was [%s]\n",
                            subPrefix, formatter.getFormatterName(), wrapperValue.getValue());
                    return false;
                }
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
        } else if (schemas.contains(fieldType)) {
            SubTypeViaString subTypeViaString = fieldType.getAnnotation(SubTypeViaString.class);
            if (subTypeViaString != null) {
                String propertyValue = (String) wrapperValue.getPropertyValue(subTypeViaString.property());
                Class<?> subType = Stream.of(subTypeViaString.types())
                        .filter(t -> t.value().equals(propertyValue))
                        .map(SubTypeViaString.Type::type)
                        .findFirst().get();
                return isRightObject(subType, wrapperValue, subPrefix);
            } else
                return isRightObject(fieldType, wrapperValue, subPrefix);
        } else if (Iterable.class.isAssignableFrom(fieldType)) {
            int index = 0;
            Type elementType = getGenericParams(genericType, 0).orElseThrow(() ->
                    new IllegalArgumentException(subPrefix + " should be generic type"));
            Class<?> subType = guessType(subPrefix, elementType);
            for (WrappedObject wrappedElement : wrapperValue.getWrappedList()) {
                if (!isRightObject(String.format("%s[%d]", subPrefix, index++), wrappedElement, subType, elementType))
                    return false;
            }
        } else if (Map.class.isAssignableFrom(fieldType)) {
            Type elementType = getGenericParams(genericType, 1).orElseThrow(() ->
                    new IllegalArgumentException(subPrefix + " should be generic type"));
            Class<?> subType = guessType(subPrefix, elementType);
            for (String key : wrapperValue.getPropertyReaderNames()) {
                if (!isRightObject(subPrefix + "." + key, wrapperValue.getPropertyValueWrapper(key), subType, elementType))
                    return false;
            }
        }
        return true;
    }

    public CompilingContextBuilder registerSchema(String name, Predicate<WrappedObject> predicate) {
        typeDefinitions.put(name, o -> requiredType(o != null &&
                predicate.test(wrap(o)), () -> o));
        return this;
    }

    public WrappedObject wrap(Object o) {
        return new WrappedObject(o, propertyAccessors, listAccessors);
    }

    public <T> CompilingContextBuilder registerPropertyAccessor(Class<T> type, PropertyAccessor<T> propertyAccessor) {
        propertyAccessors.put(type, propertyAccessor);
        return this;
    }

    public <T> CompilingContextBuilder registerListAccessor(Class<T> type, ListAccessor<T> listAccessor) {
        listAccessors.put(type, listAccessor);
        return this;
    }
}
