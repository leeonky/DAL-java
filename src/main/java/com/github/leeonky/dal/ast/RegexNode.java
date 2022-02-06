package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.RuntimeException;

import java.util.regex.Pattern;

import static java.lang.String.format;

public class RegexNode extends DALNode {
    private final Pattern pattern;

    public RegexNode(String regex) {
        pattern = Pattern.compile(regex);
    }

    @Override
    public String inspect() {
        return format("/%s/", pattern.toString());
    }

    private boolean matches(String actual, Data input) {
        return AssertionFailure.assertRegexMatches(pattern, actual, getPositionBegin(), input);
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Equal operator, RuntimeContextBuilder.DALRuntimeContext context) {
        Data actual = actualNode.evaluateData(context);
        if (actual.getInstance() instanceof String)
            return matches((String) actual.getInstance(), actual);
        throw new RuntimeException("Operator = before regex need a string input value", operator.getPosition());
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Matcher operator, RuntimeContextBuilder.DALRuntimeContext context) {
        Data actual = actualNode.evaluateData(context);
        return matches((String) actual.convert(String.class).getInstance(), actual);
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof RegexNode && ((RegexNode) o).pattern.toString().equals(pattern.toString());
    }
}
