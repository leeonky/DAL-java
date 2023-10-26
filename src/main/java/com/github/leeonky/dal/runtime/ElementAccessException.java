package com.github.leeonky.dal.runtime;

public class ElementAccessException extends java.lang.RuntimeException {
    private final int index;
    private final PropertyAccessException exception;

    public ElementAccessException(int index, PropertyAccessException exception) {
        this.index = index;
        this.exception = exception;
    }

    public RuntimeException toDalError(int position) {
        return exception.toDalError(String.format("Mapping element[%d]:\n", index), position);
    }

    public int index() {
        return index;
    }
}
