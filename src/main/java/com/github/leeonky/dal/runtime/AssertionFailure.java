package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.NumberType;

import java.util.Set;
import java.util.regex.Pattern;

import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class AssertionFailure extends DalException {
    public AssertionFailure(String message, int position) {
        super(message.trim(), position);
    }

    public static boolean assertMatchNull(Data actual, int position) {
        if (!actual.isNull())
            throw new AssertionFailure(format("Expected to match: null\nActual: %s", actual.inspect()), position);
        return true;
    }

    public static boolean assertMatch(Data expected, Data actual, int position, NumberType numberType) {
        Object expectedValue = expected.getInstance();
        Object actualValue = actual.getInstance();
        if (expectedValue instanceof Number && actualValue instanceof Number)
            return numberType.compare((Number) expectedValue, (Number) actualValue) == 0
                    || raiseNotMatchError(expected, actual, position);
        else {
            Data converted = convert(actual, expectedValue.getClass(), position);
            return Calculator.equals(converted, expected) || (converted.getInstance() == actual.getInstance()
                    ? raiseNotMatchError(expected, actual, position) : raiseNotMatchErrorWithConvertedValue(expected,
                    actual, position, converted));
        }
    }

    private static Data convert(Data actual, Class<?> targetType, int position) {
        try {
            return actual.convert(targetType);
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), position);
        }
    }

    private static boolean raiseNotMatchErrorWithConvertedValue(Data expected, Data actual, int position, Data converted) {
        throw new AssertionFailure(format("Expected to match: %s\nActual: %s converted from: %s",
                expected.inspect().trim(), converted.inspect().trim(), actual.inspect().trim()), position);
    }

    private static boolean raiseNotMatchError(Data expected, Data actual, int position) {
        throw new AssertionFailure(format("Expected to match: %s\nActual: %s",
                expected.inspect().trim(), actual.inspect().trim()), position);
    }

    public static void assertUnexpectedFields(Set<Object> dataFields, String element, int position) {
        if (!dataFields.isEmpty())
            throw new AssertionFailure(format("Unexpected fields %s%s",
                    dataFields.stream().map(s -> s instanceof String ? format("`%s`", s) : s.toString())
                            .collect(joining(", ")), element.isEmpty() ? "" : " in " + element), position);
    }

    public static boolean assertEquals(Data expected, Data actual, int position) {
        if (!Calculator.equals(actual, expected))
            throw new AssertionFailure(format("Expected to be equal to: %s\nActual: %s",
                    expected.inspect().trim(), actual.inspect().trim()), position);
        return true;
    }

    public static boolean assertRegexMatches(Pattern pattern, String actual, int position) {
        if (!pattern.matcher(actual).matches())
            throw new AssertionFailure(format("Expected to match: /%s/\nActual: <%s>", pattern, actual), position);
        return true;
    }

    public static boolean assertRegexMatches(Pattern pattern, String converted, Data input, int position) {
        if (!pattern.matcher(converted).matches())
            throw new AssertionFailure(format("Expected to match: /%s/\nActual: <%s> converted from: %s", pattern,
                    converted, input.inspect()), position);
        return true;
    }
}