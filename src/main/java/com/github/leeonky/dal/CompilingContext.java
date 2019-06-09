package com.github.leeonky.dal;

import java.util.LinkedList;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

public class CompilingContext {
    private final Map<Class<?>, CheckedBiFunction<?, String, Object>> propertyAccessors;
    private final LinkedList<Object> wrappedValueStack = new LinkedList<>();
    private final Map<String, Function<Object, Object>> types;

    public CompilingContext(Object inputValue, Map<Class<?>, CheckedBiFunction<?, String, Object>> propertyAccessors,
                            Map<String, Function<Object, Object>> types) {
        this.propertyAccessors = propertyAccessors;
        wrappedValueStack.add(inputValue);
        this.types = types;
    }

    public Object getInputValue() {
        return wrappedValueStack.getLast();
    }

    public void wrapInputValue(Object value) {
        wrappedValueStack.add(value);
    }

    public void unWrapLastInputValue() {
        wrappedValueStack.removeLast();
    }

    public Optional<CheckedBiFunction<?, String, Object>> customerGetter(Object instance) {
        return propertyAccessors.entrySet().stream()
                .filter(e -> e.getKey().isInstance(instance))
                .findFirst()
                .map(Map.Entry::getValue);
    }

    public Map<String, Function<Object, Object>> getTypes() {
        return types;
    }
}
