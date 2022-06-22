package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.AssertionFailure.assertUnexpectedFields;
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
    protected boolean verify(Data data, DALOperator.Equal operator, DALRuntimeContext context, DALNode actualNode) {
        checkNull(data);
        return data.newBlockScope(() -> {
            verificationExpressions.forEach(expression -> expression.evaluate(context));
            assertUnexpectedFields(collectUnexpectedFields(data, context), actualNode.inspect(), operator.getPosition());
            return true;
        });
    }

    private Set<Object> collectUnexpectedFields(Data data, DALRuntimeContext context) {
        return new LinkedHashSet<Object>(data.getFieldNames()) {{
            removeAll(collectFields(data));
            //                    TODO object key *********************
            removeAll(context.removeVerifiedFlattenProperties(data));
        }};
    }

    @Override
    protected boolean verify(Data data, DALOperator.Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        if (verificationExpressions.isEmpty() && !isObjectWildcard)
            throw new SyntaxException("Should use `{...}` to verify any non null object", getPositionBegin());
        checkNull(data);
        return data.newBlockScope(() -> {
            verificationExpressions.forEach(expression -> expression.evaluate(context));
            return true;
        });
    }

    private void checkNull(Data data) {
        if (data.isNull())
            throw new AssertionFailure("The input value is null", getPositionBegin());
    }

    private Set<Object> collectFields(Data data) {
        return verificationExpressions.stream().flatMap(expression -> {
            DALNode keyNode = ((DALExpression) ((DALExpression) expression).getLeftOperand()).getRightOperand();
            if (keyNode instanceof PropertyThis) {
                DALNode expectedNode = ((DALExpression) expression).getRightOperand();
                if (expectedNode instanceof ObjectScopeNode)
                    return ((ObjectScopeNode) expectedNode).collectFields(data).stream();
            }
            return Stream.of(data.firstFieldFromAlias(expression.getRootSymbolName()));
        }).collect(Collectors.toSet());
    }
}
