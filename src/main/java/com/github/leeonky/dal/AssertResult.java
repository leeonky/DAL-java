package com.github.leeonky.dal;

public class AssertResult {
    private boolean passed = true;
    private String message;

    public boolean isPassed() {
        return passed;
    }

    public String getMessage() {
        return message;
    }

    AssertResult setMessage(String message) {
        passed = false;
        this.message = message;
        return this;
    }
}
