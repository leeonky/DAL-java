package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.NumberType;

import java.util.Set;
import java.util.regex.Pattern;

import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class AssertionFailure extends DalException {
    public AssertionFailure(String message, int position) {
        super(message, position);
    }

    public static void assertListSize(int expected, int actual, int position) {
        if (expected != actual)
            throw new AssertionFailure(format("Expecting list size to be <%d> but was <%d>", expected, actual), position);
    }

    public static boolean assertMatchNull(Data actual, int position) {
        if (!actual.isNull())
            throw new AssertionFailure(format("Expecting %sto match null but was not", actual.inspect()), position);
        return true;
    }

    public static boolean assertMatch(Data expected, Data actual, int position, NumberType numberType) {
        Object expectedValue = expected.getInstance();
        Object actualValue = actual.getInstance();
        if (expectedValue instanceof Number && actualValue instanceof Number)
            return numberType.compare((Number) expectedValue, (Number) actualValue) == 0
                    || raiseNotMatchError(expected, actual, position);
        else {
            Data converted = actual.convert(expectedValue.getClass());
            return Calculator.equals(converted, expected) ||
                    (converted.getInstance() == actual.getInstance() ? raiseNotMatchError(expected, actual, position)
                            : raiseNotMatchErrorWithConvertedValue(expected, actual, position, converted));
        }
    }

    private static boolean raiseNotMatchErrorWithConvertedValue(Data expected, Data actual, int position, Data converted) {
        throw new AssertionFailure(format("Expecting %sConvert to: %sto match %sbut was not",
                actual.inspect(), converted.inspect(), expected.inspect()), position);
    }

    private static boolean raiseNotMatchError(Data expected, Data actual, int position) {
        throw new AssertionFailure(format("Expecting %sto match %sbut was not",
                actual.inspect(), expected.inspect()), position);
    }

    public static void assertUnexpectedFields(Set<String> dataFields, String element, int position) {
        if (!dataFields.isEmpty())
            throw new AssertionFailure(format("Unexpected fields %s%s",
                    dataFields.stream().map(s -> format("`%s`", s)).collect(joining(", ")),
                    element.isEmpty() ? "" : " in " + element), position);
    }

    public static boolean assertEquals(Data expected, Data actual, int position) {
        if (!Calculator.equals(actual, expected))
            throw new AssertionFailure(format("Expecting %sto be equal to %sbut was not",
                    actual.inspect(), expected.inspect()), position);
        return true;
    }

    public static boolean assertRegexMatches(Pattern pattern, String actual, int position, Data input) {
        if (!pattern.matcher(actual).matches())
            throw new AssertionFailure(format("Expecting %sto match /%s/ but was not", input.inspect(), pattern), position);
        return true;
    }
}
