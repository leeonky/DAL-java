package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.DALOperator.PropertyImplicit;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.AssertionFailure.assertListSize;
import static com.github.leeonky.dal.ast.SymbolNode.Type.BRACKET;
import static com.github.leeonky.interpreter.FunctionUtil.eachWithIndex;
import static com.github.leeonky.interpreter.FunctionUtil.mapWithIndex;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class ListScopeNode extends DALNode {
    private List<DALNode> expressions__;
    private List<DALNode> inputExpressions;
    private List<Clause<DALRuntimeContext, DALNode>> expressionFactories;
    private final Type type;
    private final boolean multiLineList;

    public ListScopeNode(List<Clause<DALRuntimeContext, DALNode>> expressionFactories, boolean multiLineList) {
        type = guessType(expressionFactories);
        this.expressionFactories = expressionFactories;
        this.multiLineList = multiLineList;

        // just use side effect to check list syntax: [1 ... 2 ... ]
        getExpressions(0);
    }

    private List<DALNode> getExpressions(int firstIndex) {
        return expressions__ != null ? expressions__ : type.checkElements(getInputExpressions(firstIndex)).stream()
                .filter(node -> !(node instanceof ListEllipsisNode)).collect(toList());
    }

    private List<DALNode> getInputExpressions(int firstIndex) {
        return inputExpressions != null ? inputExpressions : mapWithIndex(expressionFactories.stream(),
                (i, clause) -> clause.makeExpression(new DALExpression(InputNode.INSTANCE, new PropertyImplicit(),
                        new SymbolNode(type.indexOfNode(firstIndex, i, expressionFactories.size()), BRACKET))))
                .collect(toList());
    }

    public ListScopeNode(List<DALNode> inputExpressions, boolean multiLineList, Type type) {
        expressions__ = this.inputExpressions = new ArrayList<>(inputExpressions);
        this.multiLineList = multiLineList;
        this.type = type;
    }

    public ListScopeNode(List<Clause<DALRuntimeContext, DALNode>> expressionFactories) {
        this(expressionFactories, false);
    }

    private Type guessType(List<Clause<DALRuntimeContext, DALNode>> expressionFactories) {
        if (expressionFactories.size() > 0 && isListEllipsis(expressionFactories.get(expressionFactories.size() - 1)))
            return Type.FIRST_N_ITEMS;
        else if (expressionFactories.size() > 0 && isListEllipsis(expressionFactories.get(0)))
            return Type.LAST_N_ITEMS;
        return Type.ALL_ITEMS;
    }

    @Deprecated
    public ListScopeNode() {
        this(Collections.emptyList());
    }

    // Only for test
//    TODO remove
    public List<DALNode> getExpressions() {
        return getExpressions(0);
    }

    @Override
    public String inspect() {
        return getInputExpressions(0).stream().map(DALNode::inspect).collect(joining(", ", "[", "]"));
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Equal operator, DALRuntimeContext context) {
        return judgeAll(context, actualNode.evaluateData(context));
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Matcher operator, DALRuntimeContext context) {
        return judgeAll(context, actualNode.evaluateData(context));
    }

    public boolean judgeAll(DALRuntimeContext context, Data data) {
        if (!data.isList())
            throw new RuntimeException(format("Cannot compare%sand list", data.inspect()), getPositionBegin());
        List<DALNode> expressions = getExpressions(data.getListFirstIndex());
        if (type == Type.ALL_ITEMS)
            assertListSize(expressions.size(), data.getListSize(), getPositionBegin());
        return context.newBlockScope(data, () -> assertElementExpressions(context, expressions));
    }

    private boolean assertElementExpressions(DALRuntimeContext context, List<DALNode> expressions) {
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

    private boolean isListEllipsis(Clause<DALRuntimeContext, DALNode> clause) {
        return clause.makeExpression(null) instanceof ListEllipsisNode;
    }

    public enum Type {
        ALL_ITEMS {
            @Override
            protected Stream<DALNode> toChecking(List<DALNode> inputExpressions) {
                return inputExpressions.stream();
            }
        }, FIRST_N_ITEMS {
            @Override
            protected Stream<DALNode> toChecking(List<DALNode> inputExpressions) {
                return inputExpressions.stream().limit(inputExpressions.size() - 1);
            }
        }, LAST_N_ITEMS {
            @Override
            int indexOfNode(int firstIndex, int index, int count) {
                return index - count;
            }

            @Override
            protected Stream<DALNode> toChecking(List<DALNode> inputExpressions) {
                return inputExpressions.stream().skip(1);
            }
        };

        int indexOfNode(int firstIndex, int index, int count) {
            return index + firstIndex;
        }

        protected abstract Stream<DALNode> toChecking(List<DALNode> inputExpressions);

        public List<DALNode> checkElements(List<DALNode> inputExpressions) {
            toChecking(inputExpressions).forEach(node -> {
                if (node instanceof ListEllipsisNode)
                    throw new SyntaxException("unexpected token", node.getPositionBegin());
            });
            return inputExpressions;
        }
    }
}
