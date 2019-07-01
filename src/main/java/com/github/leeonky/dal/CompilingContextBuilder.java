package com.github.leeonky.dal;

import com.github.leeonky.dal.format.*;
import com.github.leeonky.dal.token.IllegalTypeException;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.util.*;

import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class CompilingContextBuilder {
    private final TypeData<PropertyAccessor> propertyAccessors = new TypeData<>();
    private final Map<String, Function<Object, Object>> typeDefinitions = new LinkedHashMap<>();
    private final TypeData<ListAccessor> listAccessors = new TypeData<>();
    private final Set<Class<?>> schemas = new HashSet<>();

    public CompilingContextBuilder() {
        registerValueFormat(new PositiveInteger());
        registerValueFormat(new URL());
        registerValueFormat(new Instant());
        registerValueFormat("String", new StringType());
    }

    public static <T> T requiredType(boolean rightType, Supplier<T> supplier) {
        if (rightType)
            return supplier.get();
        throw new IllegalTypeException();
    }

    public CompilingContextBuilder registerValueFormat(Formatter formatter) {
        return registerValueFormat(formatter.getClass().getSimpleName(), formatter);
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
        return registerSchema(name, bw -> isRightObject(clazz, bw));
    }

    @SuppressWarnings("unchecked")
    private boolean isRightObject(Class<?> clazz, WrappedObject bw) {
        Set<Member> members = BeanUtil.findPropertyReaders(clazz);
        Set<String> expectedFields = members.stream().map(Member::getName).collect(Collectors.toSet());
        Set<String> actualFields = bw.getPropertyReaderNames();
        for (String f : actualFields) {
            if (!expectedFields.contains(f)) {
                System.err.printf("Unexpected field %s for type %s[%s]\n", f, clazz.getSimpleName(), clazz.getName());
                return false;
            }
        }
        for (Member member : members) {
            Field field = (Field) member;
            boolean allowNull = field.getAnnotation(AllowNull.class) != null;
            if (!allowNull && !actualFields.contains(field.getName())) {
                System.err.printf("Expected field %s for type %s[%s], but does not exist\n", field.getName(), clazz.getSimpleName(), clazz.getName());
                return false;
            }
            Class<?> fieldType = field.getType();
            WrappedObject propertyValueWrapper = bw.getPropertyValueWrapper(member.getName());
            if (Formatter.class.isAssignableFrom(fieldType)) {
                try {
                    Formatter formatter = (Formatter) fieldType.getConstructor().newInstance();
                    if (allowNull && propertyValueWrapper.isNull())
                        continue;
                    if (!formatter.isValidValue(propertyValueWrapper.getValue()))
                        return false;
                } catch (Exception e) {
                    throw new IllegalStateException(e);
                }
            } else if (schemas.contains(fieldType)) {
                if (!isRightObject(fieldType, propertyValueWrapper))
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
