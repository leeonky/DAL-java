package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.util.Converter;
import com.github.leeonky.util.NumberUtil;

import java.util.Set;
import java.util.regex.Pattern;

import static com.github.leeonky.dal.ast.ConstNode.inspectValue;
import static com.github.leeonky.util.BeanClass.getClassName;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

//TODO checking null should use dataObject isnull
//TODO should not wrap '' with string
public class AssertionFailure extends DalException {

    public AssertionFailure(String message, int position) {
        super(message, position);
    }

    public static void assertListSize(int expected, int actual, int position) {
        if (expected != actual)
            throw new AssertionFailure(format("expected list size <%d> but was <%d>", expected, actual), position);
    }

    public static boolean assertMatchNull(Object actual, int position) {
        if (actual != null)
            throw new AssertionFailure(format("expected%smatches null but was not", inspectTypeValue(actual)), position);
        return true;
    }

    public static boolean assertMatch(Object expected, Object actual, int position, Converter converter) {
        if (expected instanceof Number && actual instanceof Number ?
                NumberUtil.compare((Number) expected, (Number) actual, converter) != 0
                : !Calculator.equals(converter.convert(expected.getClass(), actual), expected))
            throw new AssertionFailure(format("expected%smatches%sbut was not",
                    inspectTypeValue(actual), inspectTypeValue(expected)), position);
        return true;
    }

    public static String inspectTypeValue(Object value) {
        if (value == null)
            return " null ";
        return String.format(" %s\n<%s>\n", getClassName(value), inspectValue(value));
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

    public static boolean assertEquals(Object expected, Object actual, int position) {
        if (!Calculator.equals(actual, expected))
            throw new AssertionFailure(format("expected%sequal to%sbut was not",
                    inspectTypeValue(actual), inspectTypeValue(expected)), position);
        return true;
    }

    public static boolean assertRegexMatches(Pattern pattern, String actual, int position, String typeName) {
        if (!pattern.matcher(actual).matches())
            throw new AssertionFailure(format("expected %s <%s> matches /%s/ but was not",
                    typeName, inspectValue(actual), pattern), position);
        return true;
    }
}
