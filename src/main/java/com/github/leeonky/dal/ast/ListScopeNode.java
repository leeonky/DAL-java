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
import java.util.Comparator;
import java.util.List;

import static com.github.leeonky.dal.ast.AssertionFailure.assertListSize;
import static com.github.leeonky.dal.ast.SymbolNode.Type.BRACKET;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class ListScopeNode extends DALNode {
    private List<DALNode> expressions__;
    private List<DALNode> inputExpressions;
    private List<Clause<DALRuntimeContext, DALNode>> expressionFactories;
    private final Type type;
    private final boolean multiLineList;
    private final Comparator<Object> listComparator;

    public ListScopeNode(List<Clause<DALRuntimeContext, DALNode>> expressionFactories, boolean multiLineList,
                         Comparator<Object> listComparator) {
        type = guessType(expressionFactories);
        this.expressionFactories = expressionFactories;
        this.multiLineList = multiLineList;
        this.listComparator = listComparator;
    }

    public ListScopeNode(List<DALNode> inputExpressions, boolean multiLineList, Type type,
                         Comparator<Object> listComparator) {
        expressions__ = this.inputExpressions = new ArrayList<>(inputExpressions);
        this.multiLineList = multiLineList;
        this.type = type;
        this.listComparator = listComparator;
    }

    public ListScopeNode(List<Clause<DALRuntimeContext, DALNode>> expressionFactories) {
        this(expressionFactories, false, SortSequenceNode.NOP_COMPARATOR);
    }

    private List<DALNode> getExpressions(int firstIndex) {
        return expressions__ != null ? expressions__ : getInputExpressions(firstIndex).stream()
                .filter(node -> !(node instanceof ListEllipsisNode)).collect(toList());
    }

    private List<DALNode> getInputExpressions(int firstIndex) {
        if (inputExpressions != null)
            return inputExpressions;
        return new ArrayList<DALNode>() {{
            for (int i = 0; i < expressionFactories.size(); i++)
                add(expressionFactories.get(i).expression(new DALExpression(InputNode.INSTANCE, new PropertyImplicit(),
                        new SymbolNode(type.indexOfNode(firstIndex, i, expressionFactories.size()), BRACKET))));
        }};
    }

    private Type guessType(List<Clause<DALRuntimeContext, DALNode>> expressionFactories) {
        List<Boolean> isListEllipsis = expressionFactories.stream().map(this::isListEllipsis).collect(toList());
        long ellipsesCount = isListEllipsis.stream().filter(Boolean::booleanValue).count();
        if (ellipsesCount > 0) {
            if (ellipsesCount == 1) {
                if (isListEllipsis.get(0))
                    return Type.LAST_N_ITEMS;
                if (isListEllipsis.get(isListEllipsis.size() - 1))
                    return Type.FIRST_N_ITEMS;
            } else if (ellipsesCount == 2 && isListEllipsis.get(0) && isListEllipsis.get(isListEllipsis.size() - 1))
                return Type.CONTAINS;
            throw new SyntaxException("Invalid ellipsis", expressionFactories.get(isListEllipsis.lastIndexOf(true))
                    .expression(null).getOperandPosition());
        }
        return Type.ALL_ITEMS;
    }

    @Override
    public String inspect() {
        if (type == Type.CONTAINS)
            return expressionFactories.stream().map(clause -> clause.expression(InputNode.INSTANCE).inspect())
                    .collect(joining(", ", "[", "]"));
        return getInputExpressions(0).stream().map(DALNode::inspect).collect(joining(", ", "[", "]"));
    }

    @Override
    protected boolean verify(Data actual, DALOperator.Equal operator, DALRuntimeContext context, DALNode actualNode) {
        return verifyAll(context, actual);
    }

    @Override
    protected boolean verify(Data actual, DALOperator.Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        return verifyAll(context, actual);
    }

    private boolean verifyAll(DALRuntimeContext context, Data data) {
        if (!data.isList())
            throw new RuntimeException(format("Cannot compare %sand list", data.inspect()), getPositionBegin());
        data.setListComparator(listComparator);
        int listFirstIndex = data.getListFirstIndex();
        if (type == Type.CONTAINS) {
//            TODO raise error when index list(expressionFactories == null)
            int elementIndex = 0;
            for (Clause<DALRuntimeContext, DALNode> clause : expressionFactories.subList(1, expressionFactories.size() - 1))
                while (!verifyContains(context, data, elementIndex++, clause, listFirstIndex)) ;
            return true;
        } else {
            List<DALNode> expressions = getExpressions(listFirstIndex);
            if (type == Type.ALL_ITEMS)
                assertListSize(expressions.size(), data.getListSize(), getPositionBegin());
            return context.newBlockScope(data, () -> assertElementExpressions(context, expressions));
        }
    }

    private boolean verifyContains(DALRuntimeContext context, Data data, int index,
                                   Clause<DALRuntimeContext, DALNode> clause, int listFirstIndex) {
        if (index == data.getListSize())
            throw new AssertionFailure("No such element", clause.expression(InputNode.INSTANCE).getOperandPosition());
        try {
            return context.newBlockScope(data, () -> (boolean) clause.expression(new DALExpression(InputNode.INSTANCE,
                    new PropertyImplicit(), new SymbolNode(index + listFirstIndex, BRACKET))).evaluate(context));
        } catch (AssertionFailure ignore) {
            return false;
        }
    }

    private boolean assertElementExpressions(DALRuntimeContext context, List<DALNode> expressions) {
        if (multiLineList)
            for (int i = 0; i < expressions.size(); i++) {
                DALNode expression = expressions.get(i);
                try {
                    expression.evaluate(context);
                } catch (DalException dalException) {
                    throw new ElementAssertionFailure(i, dalException);
                }
            }
        else
            expressions.forEach(expression -> expression.evaluate(context));
        return true;
    }

    private boolean isListEllipsis(Clause<DALRuntimeContext, DALNode> clause) {
        return clause.expression(null) instanceof ListEllipsisNode;
    }

    public enum Type {
        ALL_ITEMS, FIRST_N_ITEMS, LAST_N_ITEMS {
            @Override
            int indexOfNode(int firstIndex, int index, int count) {
                return index - count;
            }

        }, CONTAINS;

        int indexOfNode(int firstIndex, int index, int count) {
            return index + firstIndex;
        }
    }
}
