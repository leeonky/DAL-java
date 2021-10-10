package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.util.Converter;
import com.github.leeonky.util.NumberUtil;

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

    //    TODO update message value+type
    public static boolean assertMatch(Object expect, Object actual, int position, Converter converter) {
        if (expect instanceof Number && actual instanceof Number ?
                NumberUtil.compare((Number) expect, (Number) actual, converter) != 0
                : !Calculator.equals(converter.convert(expect.getClass(), actual), expect))
            throw new AssertionFailure(format("expected [%s] matches [%s] but was not",
                    inspectValue(actual), inspectValue(expect)), position);
        return true;
    }

    public static void assertUnexpectedFields(Set<String> dataFields, int position) {
        if (!dataFields.isEmpty())
            throw new AssertionFailure("unexpected fields " +
                    dataFields.stream().map(s -> format("`%s`", s)).collect(joining(", ")), position);
    }

    public static void assertUnexpectedFields(Set<String> dataFields, String element, int position) {
        if (!dataFields.isEmpty())
            throw new AssertionFailure(format("unexpected fields %s in %s",
                    dataFields.stream().map(s -> format("`%s`", s)).collect(joining(", ")), element), position);
    }

    public static boolean assertEquals(Object actual, Object expected, int position) {
        if (!Calculator.equals(actual, expected))
//            TODO refactor message type+value
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
