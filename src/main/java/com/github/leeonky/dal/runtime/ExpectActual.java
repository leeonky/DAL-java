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

    public boolean objectNotEquals() {
        return !Calculator.equals(actual, expected);
    }

    public String shouldEqualTo() {
        return format("Expected to be equal to: %s\nActual: %s", expected.inspect().trim(), actual.inspect().trim());
    }

    public boolean actualNotNull() {
        return !actual.isNull();
    }

    public String shouldMatchNull() {
        return format("Expected to match: null\nActual: %s", actual.inspect());
    }

    public boolean isInstanceOf(Class<?> actualType, Class<?> expectType) {
        return actualType.isInstance(actual.getInstance()) && expectType.isInstance(expected.getInstance());
    }

    public boolean isAllNumber() {
        return expected.getInstance() instanceof Number && actual.getInstance() instanceof Number;
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
        return actual.convert(expected.getInstance().getClass());
    }

    public String shouldMatch(Data converted) {
        if (converted.getInstance() == actual.getInstance()) {
            return shouldMatch();
        }
        return format("Expected to match: %s\nActual: %s converted from: %s",
                expected.inspect().trim(), converted.inspect().trim(), actual.inspect().trim());
    }

    public Object getExpectInstance() {
        return expected.getInstance();
    }

    public String cannotCompare() {
        return format("Cannot compare between %sand %s", actual.inspect(), expected.inspect());
    }

    boolean expectNull() {
        return expected.isNull();
    }

    boolean equalTo(Data actual) {
        return Calculator.equals(actual, expected);
    }
}
