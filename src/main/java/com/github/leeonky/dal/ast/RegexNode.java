package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.RuntimeException;

import java.util.regex.Pattern;

import static com.github.leeonky.dal.ast.AssertionFailure.assertRegexMatches;
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

    @Override
    protected boolean verify(Data actual, Equal operator, DALRuntimeContext context, DALNode actualNode) {
        if (actual.getInstance() instanceof String)
            return assertRegexMatches(pattern, (String) actual.getInstance(), getPositionBegin());
        throw new RuntimeException("Operator = before regex need a string input value", operator.getPosition());
    }

    @Override
    protected boolean verify(Data actual, Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        return assertRegexMatches(pattern, (String) actual.convert(String.class).getInstance(), actual, getPositionBegin());
    }
}
