package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContext;

import java.util.regex.Pattern;

import static java.lang.String.format;

public class RegexNode extends Node {
    private final Pattern pattern;

    public RegexNode(String regex) {
        pattern = Pattern.compile(regex);
    }

    @Override
    public String inspect() {
        return format("/%s/", pattern.toString());
    }

    private boolean matches(String actual) {
        return AssertionFailure.assertRegexMatches(pattern, actual, getPositionBegin());
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        Object actual = actualNode.evaluate(context);
        if (actual instanceof String)
            return matches((String) actual);
        throw new RuntimeException("Operator = before regex need a string input value", operator.getPosition());
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        return matches(context.getConverter().convert(String.class, actualNode.evaluate(context)));
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof RegexNode && ((RegexNode) o).pattern.toString().equals(pattern.toString());
    }
}
