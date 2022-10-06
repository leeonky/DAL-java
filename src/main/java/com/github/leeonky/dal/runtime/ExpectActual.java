package com.github.leeonky.dal.runtime;

import static java.lang.String.format;

public class ExpectActual {
    private final Data expected;
    private final Data actual;

    public ExpectActual(Data expected, Data actual) {
        this.expected = expected;
        this.actual = actual;
    }

    public Data getExpected() {
        return expected;
    }

    public Data getActual() {
        return actual;
    }

    public boolean objectNotEquals() {
        return !Calculator.equals(actual, expected);
    }

    public String shouldEqualTo() {
        return format("Expected to be equal to: %s\nActual: %s", expected.inspect().trim(), actual.inspect().trim());
    }
}
