package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Match;
import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import static com.github.leeonky.dal.runtime.AssertionFailure.assertUnexpectedFields;
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
    public Object evaluate(DALRuntimeContext context) {
        return NodeType.OBJECT_SCOPE;
    }

    @Override
    public Data evaluateData(DALRuntimeContext context) {
        return context.wrap(new Expectation() {
            @Override
            public Data equalTo(DALOperator operator, Data actual) {
                if (opt1(actual::isNull))
                    throw new AssertionFailure("The input value is null", getOperandPosition());
                return actual.execute(() -> {
                    verificationExpressions.forEach(expression -> expression.evaluate(context));
                    Set<Object> dataFields = collectUnexpectedFields(actual, context);
                    if (!dataFields.isEmpty())
                        throw exception(expression -> {
                            String element = expression.left().inspect();
                            return new AssertionFailure(format("Unexpected fields %s%s",
                                    dataFields.stream().map(s -> s instanceof String ? format("`%s`", s) : s.toString())
                                            .collect(joining(", ")), element.isEmpty() ? "" : " in " + element), expression.operator().getPosition());
                        });
                    return actual;
                });
            }

            @Override
            public Data matches(DALOperator operator, Data actual) {
                if (verificationExpressions.isEmpty() && !isObjectWildcard)
                    throw new SyntaxException("Should use `{...}` to verify any non null object", getPositionBegin());
                if (opt1(actual::isNull))
                    throw new AssertionFailure("The input value is null", getOperandPosition());
                return actual.execute(() -> {
                    verificationExpressions.forEach(expression -> expression.evaluate(context));
                    return actual;
                });
            }
        });
    }

    @Override
    public Data verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        Data data = evaluateActualAndCheckNull(actualNode, context);
        data.execute(() -> {
            verificationExpressions.forEach(expression -> expression.evaluate(context));
            assertUnexpectedFields(collectUnexpectedFields(data, context), actualNode.inspect(), operator.getPosition());
            return true;
        });

        return data;
//        Data placeholder = evaluateData(context);
//        return checkerVerify(context.fetchEqualsChecker(placeholder, data), placeholder, data, context);
    }

    private Data evaluateActualAndCheckNull(DALNode actualNode, DALRuntimeContext context) {
        Data data = actualNode.evaluateData(context);
        if (data.isNullWithPosition(actualNode.getOperandPosition()))
            throw new AssertionFailure("The input value is null", getOperandPosition());
        return data;
    }

    @Override
    public Data verify(DALNode actualNode, Match operator, DALRuntimeContext context) {
        if (verificationExpressions.isEmpty() && !isObjectWildcard)
            throw new SyntaxException("Should use `{...}` to verify any non null object", getPositionBegin());
        Data data = evaluateActualAndCheckNull(actualNode, context);
        data.execute(() -> {
            verificationExpressions.forEach(expression -> expression.evaluate(context));
            return data;
        });
        return data;
//        Data placeholder = evaluateData(context);
//        return checkerVerify(context.fetchMatchingChecker(placeholder, data), placeholder, data, context);
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
