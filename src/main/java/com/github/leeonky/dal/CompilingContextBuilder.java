package com.github.leeonky.dal;

import com.github.leeonky.dal.token.IllegalTypeException;
import com.github.leeonky.dal.util.BeanUtil;
import com.github.leeonky.dal.util.ListAccessor;
import com.github.leeonky.dal.util.PropertyAccessor;
import com.github.leeonky.dal.util.TypeData;

import java.net.URL;
import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;

public class CompilingContextBuilder {
    private final TypeData<PropertyAccessor> propertyAccessors = new TypeData<>();
    private final Map<String, Function<Object, Object>> typeDefinitions = new LinkedHashMap<>();
    private final TypeData<ListAccessor> listAccessors = new TypeData<>();

    public CompilingContextBuilder() {
        registerStringValueFormat(String.class);
        registerStringValueFormat(URL.class);
        registerStringValueFormat("Instant", Instant::parse);
    }

    public static <T> T requiredType(boolean rightType, Supplier<T> supplier) {
        if (rightType)
            return supplier.get();
        throw new IllegalTypeException();
    }

    public CompilingContext build(Object inputValue) {
        return new CompilingContext(inputValue, propertyAccessors, typeDefinitions, listAccessors);
    }

    public CompilingContextBuilder registerStringValueFormat(Class<?> clazz) {
        return registerStringValueFormat(clazz.getSimpleName(), clazz);
    }

    public CompilingContextBuilder registerStringValueFormat(String name, Class<?> clazz) {
        return registerStringValueFormat(name, s -> {
            try {
                return clazz.getConstructor(String.class).newInstance(s);
            } catch (Exception e) {
                throw new IllegalStateException(String.format("Failed to wrap [%s] to %s. Type Wrapper should have a constructor %s(String)", s, name, name));
            }
        });
    }

    public CompilingContextBuilder registerStringValueFormat(String name, Function<String, ?> mapper) {
        typeDefinitions.put(name, o -> requiredType(o instanceof String, () -> mapper.apply((String) o)));
        return this;
    }

    public CompilingContextBuilder registerSchema(Class<?> clazz) {
        return registerSchema(clazz.getSimpleName(), clazz);
    }

    public CompilingContextBuilder registerSchema(String name, Class<?> clazz) {
        return registerSchema(name, BeanUtil.findPropertyNames(clazz));
    }

    public CompilingContextBuilder registerSchema(String name, Set<String> schema) {
        typeDefinitions.put(name, o -> requiredType(o != null && schema.containsAll(getPropertyNames(o)), () -> o));
        return this;
    }

    @SuppressWarnings("unchecked")
    private Set<String> getPropertyNames(Object o) {
        return propertyAccessors.getData(o)
                .map(f -> f.getPropertyNames(o))
                .orElseGet(() -> {
                    if (o instanceof Map)
                        return ((Map) o).keySet();
                    return BeanUtil.findPropertyNames(o.getClass());
                });
    }

    public <T> CompilingContextBuilder registerPropertyAccessor(Class<T> type, PropertyAccessor<T> propertyAccessor) {
        propertyAccessors.put(type, propertyAccessor);
        return this;
    }

    public <T> CompilingContextBuilder registerListType(Class<T> type, ListAccessor<T> listAccessor) {
        listAccessors.put(type, listAccessor);
        return this;
    }
}
