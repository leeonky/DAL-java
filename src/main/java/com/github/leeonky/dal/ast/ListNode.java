package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.ExpressionClause;
import com.github.leeonky.dal.runtime.DataObject;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.ast.AssertionFailure.assertListSize;
import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static java.lang.String.format;
import static java.util.stream.IntStream.range;

public class ListNode extends Node {
    // TODO New type JudgementExpression
    private final List<Node> expressions;
    private final Type type;

    public ListNode(List<ExpressionClause> expressionFactories) {
        List<ExpressionClause> elementFactories = expressionFactories.stream().filter(Objects::nonNull)
                .collect(Collectors.toList());
        int size = elementFactories.size();
        type = guessType(expressionFactories);
        expressions = range(0, size).mapToObj(i -> elementFactories.get(i).makeExpression(new PropertyNode(InputNode.INSTANCE,
                type.indexOfNode(i, size), BRACKET))).collect(Collectors.toList());
    }

    private Type guessType(List<ExpressionClause> expressionFactories) {
        if (expressionFactories.size() > 0 && expressionFactories.get(expressionFactories.size() - 1) == null)
            return Type.FIRST_N_ITEMS;
        else if (expressionFactories.size() > 0 && expressionFactories.get(0) == null)
            return Type.LAST_N_ITEMS;
        else
            return Type.ALL_ITEMS;
    }

    public ListNode() {
        this(Collections.emptyList());
    }

    public List<Node> getExpressions() {
        return expressions;
    }

    @Override
    public String inspect() {
        return format("[%s%s%s]", type == Type.LAST_N_ITEMS ? "... " : "",
                expressions.stream().map(Node::inspectClause).collect(Collectors.joining(" ")),
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
        return context.newThisScope(dataObject, () -> {
            expressions.forEach(expression -> expression.evaluate(context));
            return true;
        });
    }

    private enum Type {
        ALL_ITEMS, FIRST_N_ITEMS, LAST_N_ITEMS {
            @Override
            int indexOfNode(int i, int count) {
                return i - count;
            }
        };

        int indexOfNode(int i, int count) {
            return i;
        }
    }
}
