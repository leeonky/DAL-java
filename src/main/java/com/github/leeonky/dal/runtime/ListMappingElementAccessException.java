package com.github.leeonky.dal.runtime;

public class ListMappingElementAccessException extends java.lang.RuntimeException {
    private final int index;
    private final PropertyAccessException exception;

    public ListMappingElementAccessException(int index, PropertyAccessException exception) {
        super();
        this.index = index;
        this.exception = exception;
    }

    public RuntimeException toDalError(int position) {
        return exception.toDalError(String.format("Mapping element[%d]:\n", index()), position);
    }

    public PropertyAccessException propertyAccessException() {
        return exception;
    }

    public int index() {
        return index;
    }
}
