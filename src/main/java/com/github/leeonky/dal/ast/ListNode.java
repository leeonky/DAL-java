package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.ExpressionClause;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.DataObject;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Collections;
import java.util.List;

import static com.github.leeonky.dal.ast.AssertionFailure.assertListSize;
import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static java.util.stream.IntStream.range;

public class ListNode extends Node {
    private final List<Node> expressions;
    private final List<Node> inputExpressions;
    private final Type type;
    private final boolean multiLineList;

    public ListNode(List<ExpressionClause> expressionFactories, boolean multiLineList) {
        int size = expressionFactories.size();
        type = guessType(expressionFactories);
        inputExpressions = range(0, size).mapToObj(i -> expressionFactories.get(i).makeExpression(
                new PropertyNode(InputNode.INSTANCE, type.indexOfNode(i, size), BRACKET))).collect(toList());
        expressions = inputExpressions.stream().filter(node -> !(node instanceof ListEllipsisNode)).collect(toList());
        this.multiLineList = multiLineList;
    }

    public ListNode(List<ExpressionClause> expressionFactories) {
        this(expressionFactories, false);
    }

    private Type guessType(List<ExpressionClause> expressionFactories) {
        if (expressionFactories.size() > 0 && expressionFactories.get(expressionFactories.size() - 1).isListEllipsis())
            return Type.FIRST_N_ITEMS;
        else if (expressionFactories.size() > 0 && expressionFactories.get(0).isListEllipsis())
            return Type.LAST_N_ITEMS;
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
        return inputExpressions.stream().map(Node::inspectClause).collect(joining(", ", "[", "]"));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContextBuilder.RuntimeContext context) {
        return judgeAll(context, actualNode.evaluateDataObject(context));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContextBuilder.RuntimeContext context) {
        return judgeAll(context, actualNode.evaluateDataObject(context));
    }

    public boolean judgeAll(RuntimeContextBuilder.RuntimeContext context, DataObject dataObject) {
        if (!dataObject.isList())
            throw new RuntimeException(format("Cannot compare%sand list", dataObject.inspect()), getPositionBegin());
        if (type == Type.ALL_ITEMS)
            assertListSize(expressions.size(), dataObject.getListSize(), getPositionBegin());
        return context.newThisScope(dataObject, () -> assertElementExpressions(context));
    }

    private boolean assertElementExpressions(RuntimeContextBuilder.RuntimeContext context) {
        if (multiLineList)
            expressions.forEach(expression -> {
                try {
                    expression.evaluate(context);
                } catch (DalException dalException) {
                    throw dalException.multiPosition(expression.getOperandPosition(), DalException.Position.Type.LINE);
                }
            });
        else
            expressions.forEach(expression -> expression.evaluate(context));
        return true;
    }

    private enum Type {
        ALL_ITEMS {
        }, FIRST_N_ITEMS {
        }, LAST_N_ITEMS {
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
