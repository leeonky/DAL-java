package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.ExpressionClause;
import com.github.leeonky.dal.compiler.SyntaxException;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.DataObject;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.AssertionFailure.assertListSize;
import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.runtime.FunctionUtil.eachWithIndex;
import static com.github.leeonky.dal.runtime.FunctionUtil.mapWithIndex;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class ListNode extends Node {
    private final List<Node> expressions;
    private final List<Node> inputExpressions;
    private final Type type;
    private final boolean multiLineList;

    public ListNode(List<ExpressionClause> expressionFactories, boolean multiLineList) {
        int size = expressionFactories.size();
        type = guessType(expressionFactories);
        expressions = type.checkElements(inputExpressions = mapWithIndex(expressionFactories.stream(), (i, clause)
                -> clause.makeExpression(new PropertyNode(InputNode.INSTANCE, type.indexOfNode(i, size), BRACKET)))
                .collect(toList())).stream().filter(node -> !(node instanceof ListEllipsisNode)).collect(toList());
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
            eachWithIndex(expressions.stream(), (i, expression) -> {
                try {
                    expression.evaluate(context);
                } catch (DalException dalException) {
                    throw new ElementAssertionFailure(expressions, i, dalException);
                }
            });
        else
            expressions.forEach(expression -> expression.evaluate(context));
        return true;
    }

    private enum Type {
        ALL_ITEMS {
            @Override
            protected Stream<Node> toChecking(List<Node> inputExpressions) {
                return inputExpressions.stream();
            }
        }, FIRST_N_ITEMS {
            @Override
            protected Stream<Node> toChecking(List<Node> inputExpressions) {
                return inputExpressions.stream().limit(inputExpressions.size() - 1);
            }
        }, LAST_N_ITEMS {
            @Override
            int indexOfNode(int i, int count) {
                return i - count;
            }

            @Override
            protected Stream<Node> toChecking(List<Node> inputExpressions) {
                return inputExpressions.stream().skip(1);
            }
        };

        int indexOfNode(int i, int count) {
            return i;
        }

        protected abstract Stream<Node> toChecking(List<Node> inputExpressions);

        public List<Node> checkElements(List<Node> inputExpressions) {
            toChecking(inputExpressions).forEach(node -> {
                if (node instanceof ListEllipsisNode)
                    throw new SyntaxException("unexpected token", node.getPositionBegin());
            });
            return inputExpressions;
        }
    }
}
