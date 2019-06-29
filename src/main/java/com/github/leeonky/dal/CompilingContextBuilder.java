package com.github.leeonky.dal;

import com.github.leeonky.dal.format.*;
import com.github.leeonky.dal.token.IllegalTypeException;
import com.github.leeonky.dal.util.*;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class CompilingContextBuilder {
    private final TypeData<PropertyAccessor> propertyAccessors = new TypeData<>();
    private final Map<String, Function<Object, Object>> typeDefinitions = new LinkedHashMap<>();
    private final TypeData<ListAccessor> listAccessors = new TypeData<>();
    private final Map<Class<?>, Object> formatterCache = new HashMap<>();

    public CompilingContextBuilder() {
        registerValueFormat(PositiveInteger.class);
        registerValueFormat(URL.class);
        registerValueFormat(Instant.class);
        registerValueFormat("String", StringType.class);
    }

    public static <T> T requiredType(boolean rightType, Supplier<T> supplier) {
        if (rightType)
            return supplier.get();
        throw new IllegalTypeException();
    }

    public <T extends Formatter<?>> CompilingContextBuilder registerValueFormat(Class<T> type) {
        return registerValueFormat(type.getSimpleName(), type);
    }

    @SuppressWarnings("unchecked")
    public <T extends Formatter<?>> CompilingContextBuilder registerValueFormat(String name, Class<T> type) {
        typeDefinitions.put(name, o -> {
            Formatter formatter = (Formatter) formatterCache.computeIfAbsent(type, t -> {
                try {
                    return t.getConstructor().newInstance();
                } catch (Exception e) {
                    throw new IllegalArgumentException("Could not create instance of " + type.getName());
                }
            });
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
        return registerSchema(name, bw -> BeanUtil.findPropertyReaderNames(clazz).containsAll(bw.getPropertyReaderNames()));
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
