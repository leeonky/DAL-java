package com.github.leeonky.dal;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;

public class CompilingContextBuilder {
    private final Map<Class<?>, CheckedBiFunction<?, String, Object>> propertyAccessors = new LinkedHashMap<>();
    private final Map<String, Function<Object, Boolean>> types = new LinkedHashMap<>();

    public <T> void registerPropertyAccessor(Class<T> type, CheckedBiFunction<T, String, Object> propertyAccessor) {
        propertyAccessors.put(type, propertyAccessor);
    }

    public CompilingContext build(Object inputValue) {
        return new CompilingContext(inputValue, propertyAccessors, types);
    }

    public void registerJavaLangType(Class<?> clazz) {
        types.put(clazz.getName().replace("java.lang.", ""), String.class::isInstance);
    }

    public void registerStringValueFormat(String type, Function<String, Boolean> assertion) {
        types.put(type, o ->
                (o instanceof String) && assertion.apply((String) o));
    }
}
