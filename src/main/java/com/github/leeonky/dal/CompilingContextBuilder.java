package com.github.leeonky.dal;

import com.github.leeonky.dal.token.IllegalTypeException;
import com.github.leeonky.dal.util.BeanUtil;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

public class CompilingContextBuilder {
    private final Map<Class<?>, CheckedBiFunction<?, String, Object>> propertyAccessors = new LinkedHashMap<>();
    private final Map<Class<?>, Function<?, Set<String>>> propertyCollectors = new LinkedHashMap<>();
    private final Map<String, Function<Object, Object>> types = new LinkedHashMap<>();

    public <T> CompilingContextBuilder registerPropertyAccessor(Class<T> type, CheckedBiFunction<T, String, Object> propertyAccessor) {
        propertyAccessors.put(type, propertyAccessor);
        return this;
    }

    public CompilingContext build(Object inputValue) {
        return new CompilingContext(inputValue, propertyAccessors, types);
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
        types.put(name, o -> {
            if (o instanceof String)
                return mapper.apply((String) o);
            throw new IllegalTypeException();
        });
        return this;
    }

    public CompilingContextBuilder registerSchema(Class<?> clazz) {
        return registerSchema(clazz.getSimpleName(), clazz);
    }

    public CompilingContextBuilder registerSchema(String name, Class<?> clazz) {
        return registerSchema(name, BeanUtil.findPropertyNames(clazz));
    }

    public CompilingContextBuilder registerSchema(String name, Set<String> schema) {
        types.put(name, o -> {
            if (o != null && schema.containsAll(getPropertyNames(o)))
                return o;
            throw new IllegalTypeException();
        });
        return this;
    }

    @SuppressWarnings("unchecked")
    private Set<String> getPropertyNames(Object o) {
        return propertyCollectors.entrySet().stream()
                .filter(e -> e.getKey().isInstance(o))
                .map(Map.Entry::getValue)
                .map(f -> ((Function<Object, Set<String>>) f).apply(o))
                .findFirst()
                .orElseGet(() -> {
                    if (o instanceof Map)
                        return ((Map) o).keySet();
                    return BeanUtil.findPropertyNames(o.getClass());
                });
    }

    public <T> CompilingContextBuilder registerPropertyCollector(Class<T> type, Function<T, Set<String>> collector) {
        propertyCollectors.put(type, collector);
        return this;
    }
}
