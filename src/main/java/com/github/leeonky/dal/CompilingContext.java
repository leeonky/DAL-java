package com.github.leeonky.dal;

import java.util.Map;
import java.util.Optional;

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

    public Optional<CheckedBiFunction<?, String, Object>> customerGetter(Object instance) {
        return registerTypes.entrySet().stream()
                .filter(e -> e.getKey().isInstance(instance))
                .findFirst()
                .map(Map.Entry::getValue);
    }
}
