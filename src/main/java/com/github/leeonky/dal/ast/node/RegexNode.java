package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.RuntimeException;

import java.util.regex.Pattern;

import static com.github.leeonky.dal.runtime.AssertionFailure.assertRegexMatches;
import static java.lang.String.format;

public class RegexNode extends DALNode {
    private final Pattern pattern;

    public RegexNode(String regex) {
        pattern = Pattern.compile(regex, Pattern.DOTALL);
    }

    @Override
    public String inspect() {
        return format("/%s/", pattern.toString());
    }

    @Override
    public boolean verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        Data actual = actualNode.evaluateData(context);
        if (actual.getInstance() instanceof String)
            return assertRegexMatches(pattern, (String) actual.getInstance(), getPositionBegin());
        throw new RuntimeException("Operator = before regex need a string input value", operator.getPosition());
    }

    @Override
    public boolean verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        Data actual = actualNode.evaluateData(context);
        return assertRegexMatches(pattern, (String) actual.convert(String.class).getInstance(), actual, getPositionBegin());
    }
}
