package com.github.leeonky.dal.runtime;

import static java.lang.String.format;

public class ExpectActual {
    private final Data expected;
    private final Data actual;

    public Data getActual() {
        return actual;
    }

    public Data getExpected() {
        return expected;
    }

    public ExpectActual(Data expected, Data actual) {
        this.expected = expected;
        this.actual = actual;
    }

    public boolean objectNotEquals() {
        return !Calculator.equals(actual, expected);
    }

    public boolean actualNotNull() {
        return !actual.isNull();
    }

    public boolean isInstanceOf(Class<?> actualType, Class<?> expectType) {
        return actualType.isInstance(actual.getInstance()) && expectType.isInstance(expected.getInstance());
    }

    public boolean isAllNumber() {
        return expected.getInstance() instanceof Number && actual.getInstance() instanceof Number;
    }

    public Data convertToExpectedType() {
        return actual.convert(expected.getInstance().getClass());
    }

    public Object getExpectInstance() {
        return expected.getInstance();
    }

    public boolean expectNull() {
        return expected.isNull();
    }

    public boolean equalTo(Data actual) {
        return Calculator.equals(actual, expected);
    }

    public boolean numberNotEquals() {
        return getExpected().numberNotEquals(getActual());
    }

    public String notationEqualTo() {
        return format("Expected to be equal to: %s\nActual: %s", expected.inspect(), actual.inspect());
    }

    public String notationMatch() {
        return format("Expected to match: %s\nActual: %s", expected.inspect(), actual.inspect());
    }

    public String notationMatch(Data converted) {
        return converted.getInstance() == actual.getInstance() ? notationMatch()
                : format("Expected to match: %s\nActual: %s converted from: %s", expected.inspect(),
                converted.inspect(), actual.inspect());
    }

    public String cannotCompare() {
        return format("Cannot compare between %s\nand %s", actual.inspect(), expected.inspect());
    }

    Checker defaultMatchesChecker() {
        if (expectNull())
            return ConditionalChecker.MATCH_NULL_CHECKER;
        if (isAllNumber())
            return ConditionalChecker.MATCH_NUMBER_CHECKER;
        return ConditionalChecker.MATCH_CHECKER;
    }
}
