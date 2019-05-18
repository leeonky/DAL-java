package com.github.leeonky.dal;

public class CompilingContext {
    private final Object inputValue;

    public CompilingContext(Object inputValue) {
        this.inputValue = inputValue;
    }

    public Object getInputValue() {
        return inputValue;
    }
}
