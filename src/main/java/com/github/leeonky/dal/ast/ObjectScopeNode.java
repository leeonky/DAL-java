package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.*;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.ast.AssertionFailure.assertUnexpectedFields;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class ObjectScopeNode extends DALNode {
    private final List<DALNode> expressions = new ArrayList<>();

    public ObjectScopeNode(List<DALNode> expressions) {
        this.expressions.addAll(expressions);
    }

    public ObjectScopeNode() {
        this(Collections.emptyList());
    }

    @Override
    public String inspect() {
        return format("{%s}", expressions.stream().map(DALNode::inspect).collect(joining(", ")));
    }

    @Override
    public boolean verify(DALNode actualNode, DALOperator.Equal operator, RuntimeContextBuilder.DALRuntimeContext context) {
        Data data = actualNode.evaluateData(context);
        checkNull(data);
        assertUnexpectedFields(collectUnexpectedFields(data), actualNode.inspect(), operator.getPosition());
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

    @Override
    public boolean verify(DALNode actualNode, DALOperator.Matcher operator, RuntimeContextBuilder.DALRuntimeContext context) {
        Data data = actualNode.evaluateData(context);
        checkNull(data);
        return verifyAll(context, data);
    }

    private boolean verifyAll(RuntimeContextBuilder.DALRuntimeContext context, Data data) {
        return context.newBlockScope(data, () -> {
            expressions.forEach(expression -> expression.evaluate(context));
            return true;
        });
    }
}
