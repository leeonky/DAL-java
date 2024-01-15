package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.AssertionFailure;
import com.github.leeonky.dal.runtime.CurryingMethod;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ExpectationFactory;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import static com.github.leeonky.dal.runtime.ExpressionException.exception;
import static com.github.leeonky.dal.runtime.ExpressionException.opt1;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class ObjectScopeNode extends DALNode {
    private final List<DALNode> verificationExpressions = new ArrayList<>();
    private final boolean isObjectWildcard;

    public ObjectScopeNode(List<DALNode> expressions) {
        verificationExpressions.addAll(expressions);
        isObjectWildcard = false;
    }

    public ObjectScopeNode(DALNode ignore) {
        isObjectWildcard = true;
    }

    @Override
    public String inspect() {
        return format("{%s}", isObjectWildcard ? "..." : verificationExpressions.stream().map(DALNode::inspect)
                .collect(joining(", ")));
    }

    @Override
    protected ExpectationFactory toVerify(DALRuntimeContext context) {
        return (operator, actual) -> new ExpectationFactory.Expectation() {
            @Override
            public Data matches() {
                if (verificationExpressions.isEmpty() && !isObjectWildcard)
                    throw new SyntaxException("Should use `{...}` to verify any non null object", getPositionBegin());
                if (opt1(actual::isNull))
                    throw new AssertionFailure("The input value is null", getOperandPosition());
                return actual.execute(() -> {
                    Data result = context.wrap(null);
                    for (DALNode expression : verificationExpressions)
                        result = expression.evaluateData(context);
                    return result;
                });
            }

            @Override
            public Data equalTo() {
                if (opt1(actual::isNull))
                    throw new AssertionFailure("The input value is null", getOperandPosition());
                Data execute = actual.execute(() -> {
                    Data result = context.wrap(null);
                    for (DALNode expression : verificationExpressions)
                        result = expression.evaluateData(context);
                    return result;
                });
                Set<Object> dataFields = collectUnexpectedFields(actual, context);
                if (!dataFields.isEmpty())
                    throw exception(expression -> {
                        String element = expression.left().inspect();
                        return new AssertionFailure(format("Unexpected fields %s%s",
                                dataFields.stream().map(s -> s instanceof String ? format("`%s`", s) : s.toString())
                                        .collect(joining(", ")), element.isEmpty() ? "" : " in " + element), expression.operator().getPosition());
                    });
                return execute;
            }

            @Override
            public ExpectationFactory.Type type() {
                return ExpectationFactory.Type.OBJECT;
            }
        };
    }

    private Set<Object> collectUnexpectedFields(Data data, DALRuntimeContext context) {
        return new LinkedHashSet<Object>(data.fieldNames()) {{
            Stream.concat(collectFields(data), context.collectPartialProperties(data).stream())
                    .map(obj -> convertFiled(data, obj)).forEach(this::remove);
        }};
    }

    private Object convertFiled(Data data, Object obj) {
        return data.instance() instanceof CurryingMethod ?
                ((CurryingMethod) data.instance()).convertToArgType(obj) : obj;
    }

    @Override
    public Stream<Object> collectFields(Data data) {
        return verificationExpressions.stream().flatMap(expression -> expression.collectFields(data));
    }
}
