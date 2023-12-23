package com.github.leeonky.dal.runtime;

import java.util.Set;
import java.util.regex.Pattern;

import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class AssertionFailure extends DalException {
    public AssertionFailure(String message, int position) {
        super(message, position);
    }

    @Deprecated
    public static void assertUnexpectedFields(Set<Object> dataFields, String element, int position) {
        if (!dataFields.isEmpty())
            throw new AssertionFailure(format("Unexpected fields %s%s",
                    dataFields.stream().map(s -> s instanceof String ? format("`%s`", s) : s.toString())
                            .collect(joining(", ")), element.isEmpty() ? "" : " in " + element), position);
    }

    public static void assertRegexMatches(Pattern pattern, String actual, int position) {
        if (!pattern.matcher(actual).matches())
            throw new AssertionFailure(format("Expected to match: /%s/\nActual: <%s>", pattern, actual), position);
    }

    public static void assertRegexMatches(Pattern pattern, String converted, Data input, int position) {
        if (!pattern.matcher(converted).matches())
            throw new AssertionFailure(format("Expected to match: /%s/\nActual: <%s> converted from: %s", pattern,
                    converted, input.dumpAll()), position);
    }
}
