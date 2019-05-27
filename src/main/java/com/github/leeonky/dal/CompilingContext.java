package com.github.leeonky.dal;

import java.util.LinkedHashMap;
import java.util.Map;

public class CompilingContext {
    private final Object inputValue;
    private final Map<Class<?>, CheckedBiFunction<?, String, Object>> registerTypes = new LinkedHashMap<>();

    public CompilingContext(Object inputValue) {
        this.inputValue = inputValue;
    }

    public Object getInputValue() {
        return inputValue;
    }

    public <T> void registerType(Class<T> type, CheckedBiFunction<T, String, Object> propertyAccessor) {
        registerTypes.put(type, propertyAccessor);
    }

    public Map<Class<?>, CheckedBiFunction<?, String, Object>> getRegisterTypes() {
        return registerTypes;
    }
}
