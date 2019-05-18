package com.github.leeonky.dal;

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
        return new AssertResult(false, "Expected value to [" + expression + "] but was <" + actual + ">");
    }

    public boolean isPassed() {
        return passed;
    }

    public String getMessage() {
        return message;
    }
}
