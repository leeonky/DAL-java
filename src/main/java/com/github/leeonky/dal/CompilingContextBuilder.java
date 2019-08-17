package com.github.leeonky.dal;

import com.github.leeonky.dal.format.*;
import com.github.leeonky.dal.token.IllegalTypeException;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.SubTypeViaString;
import com.github.leeonky.dal.util.ListAccessor;
import com.github.leeonky.dal.util.PropertyAccessor;
import com.github.leeonky.dal.util.TypeData;
import com.github.leeonky.dal.util.WrappedObject;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.GenericType;
import com.github.leeonky.util.PropertyReader;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
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

    @SuppressWarnings("unchecked")
    private boolean isRightObject(Class<?> clazz, WrappedObject wrappedObject, String subPrefix) {
        Class<?> type = clazz;
        SubTypeViaString subTypeViaString = clazz.getAnnotation(SubTypeViaString.class);
        if (subTypeViaString != null) {
            Object value = wrappedObject.getPropertyValue(subTypeViaString.property());
            type = Stream.of(subTypeViaString.types())
                    .filter(t -> t.value().equals(value))
                    .map(SubTypeViaString.Type::type)
                    .findFirst().orElseThrow(() -> new IllegalStateException(String.format("Cannot guess sub type through property type value[%s]", value)));
        }

        BeanClass beanClass = BeanClass.create(type);
        Map<String, PropertyReader<?>> propertyReaders = beanClass.getPropertyReaders();
        Set<String> expectedFields = propertyReaders.keySet();
        Set<String> actualFields = wrappedObject.getPropertyReaderNames();
        for (String f : actualFields) {
            if (!expectedFields.contains(f)) {
                System.err.printf("Unexpected field `%s` for type %s[%s]\n", f, type.getSimpleName(), type.getName());
                return false;
            }
        }
        for (PropertyReader propertyReader : propertyReaders.values()) {
            boolean allowNull = propertyReader.getAnnotation(AllowNull.class) != null;
            if (!allowNull && !actualFields.contains(propertyReader.getName())) {
                System.err.printf("Expected field `%s` for type %s[%s], but does not exist\n", propertyReader.getName(), type.getSimpleName(), type.getName());
                return false;
            }
            WrappedObject propertyValueWrapper = wrappedObject.getPropertyValueWrapper(propertyReader.getName());
            if (allowNull && propertyValueWrapper.isNull())
                continue;

            if (!isRightObject(subPrefix + "." + propertyReader.getName(), propertyValueWrapper, propertyReader.getGenericType()))
                return false;
        }
        return true;
    }

    @SuppressWarnings("unchecked")
    private boolean isRightObject(String subPrefix, WrappedObject wrapperValue, GenericType genericType) {
        Class<?> fieldType = genericType.getRawType();
        if (Formatter.class.isAssignableFrom(fieldType)) {
            try {
                Formatter formatter = (Formatter) fieldType.getConstructor().newInstance();
                if (!formatter.isValidValue(wrapperValue.getValue())) {
                    System.err.printf("Expected field `%s` should be in %s, but was [%s]\n",
                            subPrefix, formatter.getFormatterName(), wrapperValue.getValue());
                    return false;
                }
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
        } else if (schemas.contains(fieldType)) {
            return isRightObject(fieldType, wrapperValue, subPrefix);
        } else if (Iterable.class.isAssignableFrom(fieldType)) {
            int index = 0;
            GenericType subGenericType = genericType.getGenericTypeParameter(0).orElseThrow(() ->
                    new IllegalArgumentException(subPrefix + " should be generic type"));
            for (WrappedObject wrappedElement : wrapperValue.getWrappedList()) {
                if (!isRightObject(String.format("%s[%d]", subPrefix, index++), wrappedElement, subGenericType))
                    return false;
            }
        } else if (Map.class.isAssignableFrom(fieldType)) {
            GenericType subGenericType = genericType.getGenericTypeParameter(1).orElseThrow(() ->
                    new IllegalArgumentException(subPrefix + " should be generic type"));
            for (String key : wrapperValue.getPropertyReaderNames()) {
                if (!isRightObject(subPrefix + "." + key, wrapperValue.getPropertyValueWrapper(key), subGenericType))
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
