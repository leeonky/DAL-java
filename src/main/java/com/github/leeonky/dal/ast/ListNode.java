package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.ExpressionClause;
import com.github.leeonky.dal.compiler.SyntaxException;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.ArrayList;
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
    private List<Node> expressions__;
    private List<Node> inputExpressions;
    private List<ExpressionClause> expressionFactories;
    private final Type type;
    private final boolean multiLineList;

    public ListNode(List<ExpressionClause> expressionFactories, boolean multiLineList) {
        type = guessType(expressionFactories);
        this.expressionFactories = expressionFactories;
        this.multiLineList = multiLineList;

        // just use side effect to check list syntax: [1 ... 2 ... ]
        getExpressions(0);
    }

    private List<Node> getExpressions(int firstIndex) {
        return expressions__ != null ? expressions__ : type.checkElements(getInputExpressions(firstIndex)).stream()
                .filter(node -> !(node instanceof ListEllipsisNode)).collect(toList());
    }

    private List<Node> getInputExpressions(int firstIndex) {
        return inputExpressions != null ? inputExpressions : mapWithIndex(expressionFactories.stream(),
                (i, clause) -> clause.makeExpression(new PropertyNode(InputNode.INSTANCE,
                        type.indexOfNode(firstIndex, i, expressionFactories.size()), BRACKET))).collect(toList());
    }

    public ListNode(List<Node> inputExpressions, boolean multiLineList, Type type) {
        expressions__ = this.inputExpressions = new ArrayList<>(inputExpressions);
        this.multiLineList = multiLineList;
        this.type = type;
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

    // Only for test
    public List<Node> getExpressions() {
        return getExpressions(0);
    }

    @Override
    public String inspect() {
        return getInputExpressions(0).stream().map(Node::inspectClause).collect(joining(", ", "[", "]"));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContextBuilder.RuntimeContext context) {
        return judgeAll(context, actualNode.evaluateDataObject(context));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContextBuilder.RuntimeContext context) {
        return judgeAll(context, actualNode.evaluateDataObject(context));
    }

    public boolean judgeAll(RuntimeContextBuilder.RuntimeContext context, Data data) {
        if (!data.isList())
            throw new RuntimeException(format("Cannot compare%sand list", data.inspect()), getPositionBegin());
        List<Node> expressions = getExpressions(data.getListFirstIndex());
        if (type == Type.ALL_ITEMS)
            assertListSize(expressions.size(), data.getListSize(), getPositionBegin());
        return context.newThisScope(data, () -> assertElementExpressions(context, expressions));
    }

    private boolean assertElementExpressions(RuntimeContextBuilder.RuntimeContext context, List<Node> expressions) {
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

    public enum Type {
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
            int indexOfNode(int firstIndex, int index, int count) {
                return index - count;
            }

            @Override
            protected Stream<Node> toChecking(List<Node> inputExpressions) {
                return inputExpressions.stream().skip(1);
            }
        };

        int indexOfNode(int firstIndex, int index, int count) {
            return index + firstIndex;
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
