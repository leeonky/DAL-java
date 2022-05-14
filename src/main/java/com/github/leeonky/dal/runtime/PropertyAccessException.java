package com.github.leeonky.dal.runtime;

public class PropertyAccessException extends java.lang.RuntimeException {
    private final Object property;
    private final String message;

    public PropertyAccessException(Object property, String message) {
        this.property = property;
        this.message = message;
    }

    public PropertyAccessException(Object property, String message, Exception cause) {
        super(cause);
        this.property = property;
        this.message = message;
    }

    public RuntimeException toDalError(String prefix, int position) {
        return new RuntimeException(prefix + message, position);
    }
}
