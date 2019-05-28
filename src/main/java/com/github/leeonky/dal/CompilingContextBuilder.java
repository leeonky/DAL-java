package com.github.leeonky.dal;

import java.util.LinkedHashMap;
import java.util.Map;

public class CompilingContextBuilder {
    private final Map<Class<?>, CheckedBiFunction<?, String, Object>> registerTypes = new LinkedHashMap<>();

    public <T> void registerType(Class<T> type, CheckedBiFunction<T, String, Object> propertyAccessor) {
        registerTypes.put(type, propertyAccessor);
    }

    public CompilingContext build(Object inputValue) {
        return new CompilingContext(inputValue, registerTypes);
    }
}
