package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.DataObject;
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

    private boolean matches(String actual, DataObject input) {
        return AssertionFailure.assertRegexMatches(pattern, actual, getPositionBegin(), input);
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        DataObject actual = actualNode.evaluateDataObject(context);
        if (actual.getInstance() instanceof String)
            return matches((String) actual.getInstance(), actual);
        throw new RuntimeException("Operator = before regex need a string input value", operator.getPosition());
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        DataObject actual = actualNode.evaluateDataObject(context);
        return matches((String) actual.convert(String.class).getInstance(), actual);
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof RegexNode && ((RegexNode) o).pattern.toString().equals(pattern.toString());
    }
}
