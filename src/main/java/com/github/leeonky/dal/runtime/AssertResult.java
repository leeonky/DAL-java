package com.github.leeonky.dal.runtime;

public class AssertResult {
    private final boolean passed;
    private final String message;

    private AssertResult(boolean passed, String message) {
        this.passed = passed;
        this.message = message;
    }

    public static AssertResult passedResult() {
        return new AssertResult(true, null);
    }

    public static AssertResult failedResult(Object actual, String expression) {
        String message = expression.isEmpty() ?
                String.format("Expected root value to be [true] but was <%s>", actual)
                : String.format("Expected value to be [%s] but was <%s>", expression, actual);
        return new AssertResult(false, message);
    }

    public boolean isPassed() {
        return passed;
    }

    public String getMessage() {
        return message;
    }
}
