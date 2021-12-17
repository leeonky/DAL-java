package com.github.leeonky.dal.runtime;

public class Result {
    private static final Result EMPTY_RESULT = new Result(false, null);
    private final boolean hasResult;
    private final Object value;

    private Result(boolean hasResult, Object value) {
        this.hasResult = hasResult;
        this.value = value;
    }

    public static Result of(Object value) {
        return new Result(true, value);
    }

    public static Result empty() {
        return EMPTY_RESULT;
    }

    public boolean hasResult() {
        return hasResult;
    }

    public Object getValue() {
        return value;
    }
}
