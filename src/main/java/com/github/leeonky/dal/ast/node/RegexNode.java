package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.Expectation;
import com.github.leeonky.dal.runtime.IllegalOperationException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.regex.Pattern;

import static com.github.leeonky.dal.runtime.AssertionFailure.assertRegexMatches;
import static com.github.leeonky.dal.runtime.ExpressionException.opt2;
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
    public Data evaluateData(DALRuntimeContext context) {
        return context.wrap(new Expectation() {
            @Override
            public Data equalTo(Data actual) {
                if (actual.instance() instanceof String) {
                    assertRegexMatches(pattern, (String) actual.instance(), getPositionBegin());
                    return actual;
                }
                throw new IllegalOperationException("Operator = before regex need a string input value");
            }

            @Override
            public Data matches(Data actual) {
                assertRegexMatches(pattern, opt2(() -> (String) actual.convert(String.class).instance()), actual,
                        getPositionBegin());
                return actual;
            }
        });
    }
}
