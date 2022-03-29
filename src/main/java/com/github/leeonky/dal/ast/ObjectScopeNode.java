package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.ast.AssertionFailure.assertUnexpectedFields;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class ObjectScopeNode extends DALNode {
    private final List<DALNode> expressions = new ArrayList<>();
    private final boolean isObjectWildcard;

    public ObjectScopeNode(List<DALNode> expressions) {
        this.expressions.addAll(expressions);
        isObjectWildcard = false;
    }

    public ObjectScopeNode(DALNode node) {
        ListEllipsisNode ignore = (ListEllipsisNode) node;
        isObjectWildcard = true;
    }

    @Override
    public String inspect() {
        return format("{%s}", isObjectWildcard ? "..." : expressions.stream().map(DALNode::inspect).collect(joining(", ")));
    }

    @Override
    public boolean verify(DALNode actualNode, DALOperator.Equal operator, RuntimeContextBuilder.DALRuntimeContext context) {
        Data data = actualNode.evaluateData(context);
        checkNull(data);
        assertUnexpectedFields(collectUnexpectedFields(data), actualNode.inspect(), operator.getPosition());
        return verifyAll(context, data);
    }

    @Override
    public boolean verify(DALNode actualNode, DALOperator.Matcher operator, RuntimeContextBuilder.DALRuntimeContext context) {
        if (expressions.isEmpty() && !isObjectWildcard) {
            throw new SyntaxException("Should use `{...}` to verify any non null object", getPositionBegin());
        }
        Data data = actualNode.evaluateData(context);
        checkNull(data);
        return verifyAll(context, data);
    }

    private void checkNull(Data data) {
        if (data.isNull())
            throw new AssertionFailure("The input value is null", getPositionBegin());
    }

    private Set<String> collectUnexpectedFields(Data data) {
        return new LinkedHashSet<String>(data.getFieldNames()) {{
            removeAll(expressions.stream().map(expression ->
                    data.firstFieldFromAlias(expression.getRootSymbolName()))
                    .collect(Collectors.toSet()));
        }};
    }

    private boolean verifyAll(RuntimeContextBuilder.DALRuntimeContext context, Data data) {
        return context.newBlockScope(data, () -> {
            expressions.forEach(expression -> expression.evaluate(context));
            return true;
        });
    }
}
