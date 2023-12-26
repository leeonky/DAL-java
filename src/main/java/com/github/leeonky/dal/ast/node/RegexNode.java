package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.AssertionFailure;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.Expectation;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.regex.Pattern;

import static com.github.leeonky.dal.runtime.ExpressionException.illegalOperationRuntimeException;
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
            public Data equalTo(DALOperator operator, Data actual) {
                if (actual.instance() instanceof String) {
                    if (!pattern.matcher((String) actual.instance()).matches())
                        throw new AssertionFailure(format("Expected to match: /%s/\nActual: <%s>", pattern, actual.instance()), getPositionBegin());
                    return actual;
                }
                throw illegalOperationRuntimeException("Operator = before regex need a string input value");
            }

            @Override
            public Data matches(DALOperator operator, Data actual) {
                String converted = opt2(() -> (String) actual.convert(String.class).instance());
                if (!pattern.matcher(converted).matches())
                    throw new AssertionFailure(format("Expected to match: /%s/\nActual: <%s> converted from: %s", pattern,
                            converted, actual.dumpAll()), getPositionBegin());
                return actual;
            }
        });
    }
}
