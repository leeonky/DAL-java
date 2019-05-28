package com.github.leeonky.dal;

import java.util.Map;

public class CompilingContext {
    private final Map<Class<?>, CheckedBiFunction<?, String, Object>> registerTypes;
    private final Object inputValue;

    public CompilingContext(Object inputValue, Map<Class<?>, CheckedBiFunction<?, String, Object>> registerTypes) {
        this.registerTypes = registerTypes;
        this.inputValue = inputValue;
    }

    public Object getInputValue() {
        return inputValue;
    }

    public Map<Class<?>, CheckedBiFunction<?, String, Object>> getRegisterTypes() {
        return registerTypes;
    }
}
