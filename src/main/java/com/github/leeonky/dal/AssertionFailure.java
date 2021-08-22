package com.github.leeonky.dal;

import com.github.leeonky.dal.util.Calculator;
import com.github.leeonky.util.Converter;

import java.util.Set;
import java.util.regex.Pattern;

import static com.github.leeonky.dal.ast.ConstNode.inspectValue;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class AssertionFailure extends DalException {

    public AssertionFailure(String message, int position) {
        super(message, position);
    }

    public static void assertListSize(int expected, int actual, int position) {
        if (expected != actual)
            throw new AssertionFailure(format("expected list size [%d] but was [%d]", expected, actual), position);
    }

    public static boolean assertMatchNull(Object actual, int position) {
        if (actual != null)
            throw new AssertionFailure(format("[%s] does not match null", inspectValue(actual)),
                    position);
        return true;
    }

    public static void assertNullMatch(Object actual, int position) {
        if (actual == null)
            throw new AssertionFailure("[null] does not match non-null object", position);
    }

    public static boolean assertMatch(Object expect, Object actual, int position, Converter converter) {
        if (!Calculator.equals(converter.convert(expect.getClass(), actual), expect))
            throw new AssertionFailure(format("expected [%s] matches [%s] but was not",
                    inspectValue(actual), inspectValue(expect)), position);
        return true;
    }

    public static void assertUnexpectedFields(Set<String> dataFields, int position) {
        if (!dataFields.isEmpty())
            throw new AssertionFailure("unexpected fields " +
                    dataFields.stream().map(s -> format("`%s`", s)).collect(joining(", ")), position);
    }

    public static boolean assertEquals(Object actual, Object expected, int position) {
        if (!Calculator.equals(actual, expected))
            throw new AssertionFailure(format("expected [%s] equal to [%s] but was not",
                    inspectValue(actual), inspectValue(expected)), position);
        return true;
    }

    public static boolean assertRegexMatches(Pattern pattern, String actual, int position) {
        if (!pattern.matcher(actual).matches())
            throw new AssertionFailure(format("expected [%s] matches /%s/ but was not",
                    inspectValue(actual), pattern), position);
        return true;
    }
}
