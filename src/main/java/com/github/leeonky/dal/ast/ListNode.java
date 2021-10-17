package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.DataObject;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.ast.AssertionFailure.assertListSize;
import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static java.lang.String.format;

public class ListNode extends Node {
    // TODO New type JudgementExpression
    private final List<Expression> expressions;
    private final Type type;

    //        TODO ... should only be the first or last of list
    public ListNode(List<Expression> expressions) {
        this.expressions = expressions.stream().filter(Objects::nonNull).collect(Collectors.toList());
        if (expressions.size() > 0) {
            if (expressions.get(0) == null) {
                for (int i = 0; i < this.expressions.size(); i++) {
                    this.expressions.set(i, this.expressions.get(i).updateLeft(new PropertyNode(InputNode.INSTANCE, -1 * this.expressions.size(), BRACKET)));
                }
                type = Type.LAST_N_ITEMS;
            } else if (expressions.get(expressions.size() - 1) == null) {
                type = Type.FIRST_N_ITEMS;
            } else
                type = Type.ALL_ITEMS;
        } else
            type = Type.ALL_ITEMS;
    }

    public ListNode() {
        this(Collections.emptyList());
    }

    public List<Expression> getExpressions() {
        return expressions;
    }

    @Override
    public String inspect() {
        return format("[%s%s%s]",
                type == Type.LAST_N_ITEMS ? "... " : "",
                expressions.stream().map(Expression::getRightOperand)
                        .map(Node::inspect).collect(Collectors.joining(" ")),
                type == Type.FIRST_N_ITEMS ? " ..." : "");
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContextBuilder.RuntimeContext context) {
        return judgeAll(context, actualNode.evaluateDataObject(context));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContextBuilder.RuntimeContext context) {
        return judgeAll(context, actualNode.evaluateDataObject(context));
    }

    private boolean judgeAll(RuntimeContextBuilder.RuntimeContext context, DataObject dataObject) {
        if (!dataObject.isList())
            throw new RuntimeException(format("cannot compare%sand list", dataObject.inspect()), getPositionBegin());
        if (type == Type.ALL_ITEMS)
            assertListSize(expressions.size(), dataObject.getListSize(), getPositionBegin());
        return context.newThisScope(dataObject, () -> expressions.stream()
                .allMatch(expression -> (boolean) expression.evaluate(context)));
    }

    private enum Type {
        ALL_ITEMS, FIRST_N_ITEMS, LAST_N_ITEMS
    }
}
