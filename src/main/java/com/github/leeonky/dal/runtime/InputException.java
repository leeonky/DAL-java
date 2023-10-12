package com.github.leeonky.dal.runtime;

public class InputException extends RuntimeException {
    public InputException(Throwable throwable) {
        super("", 0, throwable);
    }

    public Throwable getInputClause() {
        return getCause().getCause();
    }
}
