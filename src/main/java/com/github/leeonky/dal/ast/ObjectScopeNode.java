package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
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
    protected boolean verify(Data data, DALOperator.Equal operator, DALRuntimeContext context, DALNode actualNode) {
        checkNull(data);
        return context.newBlockScope(data, () -> {
            expressions.forEach(expression -> expression.evaluate(context));
            assertUnexpectedFields(collectUnexpectedFields(data, context), actualNode.inspect(), operator.getPosition());
            return true;
        });
    }

    @Override
    protected boolean verify(Data data, DALOperator.Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        if (expressions.isEmpty() && !isObjectWildcard) {
            throw new SyntaxException("Should use `{...}` to verify any non null object", getPositionBegin());
        }
        checkNull(data);
        return context.newBlockScope(data, () -> {
            expressions.forEach(expression -> expression.evaluate(context));
            return true;
        });
    }

    private void checkNull(Data data) {
        if (data.isNull())
            throw new AssertionFailure("The input value is null", getPositionBegin());
    }

    private Set<String> collectUnexpectedFields(Data data, DALRuntimeContext context) {
        return new LinkedHashSet<String>(data.getFieldNames()) {{
            removeAll(expressions.stream().map(expression -> data.firstFieldFromAlias(expression.getRootSymbolName()))
                    .collect(Collectors.toSet()));
            removeAll(context.removeFlattenProperties(data));
        }};
    }
}
