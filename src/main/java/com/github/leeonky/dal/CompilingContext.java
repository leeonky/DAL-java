package com.github.leeonky.dal;

import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

public class CompilingContext {
    private final Map<Class<?>, CheckedBiFunction<?, String, Object>> propertyAccessors;
    private final Object inputValue;
    private final Map<String, Function<Object, Boolean>> types;

    public CompilingContext(Object inputValue, Map<Class<?>, CheckedBiFunction<?, String, Object>> propertyAccessors,
                            Map<String, Function<Object, Boolean>> types) {
        this.propertyAccessors = propertyAccessors;
        this.inputValue = inputValue;
        this.types = types;
    }

    public Object getInputValue() {
        return inputValue;
    }

    public Optional<CheckedBiFunction<?, String, Object>> customerGetter(Object instance) {
        return propertyAccessors.entrySet().stream()
                .filter(e -> e.getKey().isInstance(instance))
                .findFirst()
                .map(Map.Entry::getValue);
    }

    public Map<String, Function<Object, Boolean>> getTypes() {
        return types;
    }
}
