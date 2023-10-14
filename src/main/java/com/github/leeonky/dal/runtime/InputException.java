package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.InvocationException;

public class InputException extends RuntimeException {
    public InputException(Throwable throwable) {
        super(throwable.getMessage(), 0, new InvocationException(throwable));
    }

    public Throwable getInputClause() {
        return getCause().getCause();
    }
}
