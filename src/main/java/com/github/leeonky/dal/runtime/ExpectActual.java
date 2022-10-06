package com.github.leeonky.dal.runtime;

import static java.lang.String.format;

public class ExpectActual {
    private final Data expected;
    private final Data actual;
    private final RuntimeContextBuilder.DALRuntimeContext context;

    public ExpectActual(Data expected, Data actual, RuntimeContextBuilder.DALRuntimeContext context) {
        this.expected = expected;
        this.actual = actual;
        this.context = context;
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

    public boolean actualNotNull() {
        return !getActual().isNull();
    }

    public String shouldMatchNull() {
        return format("Expected to match: null\nActual: %s", getActual().inspect());
    }

    public boolean isInstanceOf(Class<?> actualType, Class<?> expectType) {
        return actualType.isInstance(actual.getInstance()) && expectType.isInstance(expected.getInstance());
    }

    public boolean isAllNumber() {
        return getExpected().getInstance() instanceof Number && getActual().getInstance() instanceof Number;
    }

    public RuntimeContextBuilder.DALRuntimeContext getContext() {
        return context;
    }

    public boolean numberNotEquals() {
        return context.getNumberType().compare((Number) expected.getInstance(), (Number) actual.getInstance()) != 0;
    }

    public String shouldMatch() {
        return format("Expected to match: %s\nActual: %s", expected.inspect().trim(), actual.inspect().trim());
    }

    public Data convertToExpectedType() {
        return getActual().convert(getExpected().getInstance().getClass());
    }

    public String shouldMatch(Data converted) {
        String message1;
        if (converted.getInstance() == getActual().getInstance()) {
            message1 = shouldMatch();
        } else
            message1 = format("Expected to match: %s\nActual: %s converted from: %s",
                    getExpected().inspect().trim(), converted.inspect().trim(), getActual().inspect().trim());
        return message1;
    }

    Object getExpectInstance() {
        return getExpected().getInstance();
    }
}
