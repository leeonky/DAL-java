package com.github.leeonky.dal.runtime;

public class RuntimeException extends DalException {
    private final Throwable cause;

    public RuntimeException(String message, int position) {
        this(message, position, null);
    }

    public RuntimeException(String message, int position, Throwable cause) {
        super(message, position);
        this.cause = cause;
    }

    @Override
    public Throwable getCause() {
        return cause;
    }
}
