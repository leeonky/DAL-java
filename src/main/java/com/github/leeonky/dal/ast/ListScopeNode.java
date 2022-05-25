package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.DALOperator.PropertyImplicit;
import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static com.github.leeonky.dal.ast.AssertionFailure.assertListSize;
import static com.github.leeonky.dal.ast.SymbolNode.Type.BRACKET;
import static java.lang.String.format;
import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.reverseOrder;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class ListScopeNode extends DALNode {
    private List<DALNode> verificationExpressions;
    private List<DALNode> inputExpressions;
    private List<Clause<DALRuntimeContext, DALNode>> inputClauses;
    private final Type type;
    private final boolean isTable;
    private final Comparator<Object> comparator;

    public ListScopeNode(List<Clause<DALRuntimeContext, DALNode>> clauses, boolean isTable,
                         Comparator<Object> comparator) {
        type = guessType(clauses);
        inputClauses = clauses;
        this.isTable = isTable;
        this.comparator = comparator;
    }

    public ListScopeNode(List<DALNode> verificationExpressions, boolean isTable, Type type,
                         Comparator<Object> comparator) {
        this.verificationExpressions = inputExpressions = new ArrayList<>(verificationExpressions);
        this.isTable = isTable;
        this.type = type;
        this.comparator = comparator;
    }

    public ListScopeNode(List<Clause<DALRuntimeContext, DALNode>> clauses) {
        this(clauses, false, SortGroupNode.NOP_COMPARATOR);
    }

    private List<DALNode> getVerificationExpressions(int firstIndex) {
        return verificationExpressions != null ? verificationExpressions : buildVerificationExpressions(firstIndex)
                .stream().filter(node -> !(node instanceof ListEllipsisNode)).collect(toList());
    }

    private List<DALNode> buildVerificationExpressions(int firstIndex) {
        if (inputExpressions != null)
            return inputExpressions;
        return new ArrayList<DALNode>() {{
            for (int i = 0; i < inputClauses.size(); i++)
                add(inputClauses.get(i).expression(getDalExpression(i, firstIndex)));
        }};
    }

    private DALExpression getDalExpression(int index, int firstIndex) {
        return new DALExpression(InputNode.INSTANCE, new PropertyImplicit(),
                new SymbolNode(type.indexOfNode(firstIndex, index, inputClauses.size()), BRACKET));
    }

    private Type guessType(List<Clause<DALRuntimeContext, DALNode>> clauses) {
        List<Boolean> isListEllipsis = clauses.stream().map(this::isListEllipsis).collect(toList());
        long ellipsesCount = isListEllipsis.stream().filter(Boolean::booleanValue).count();
        if (ellipsesCount > 0) {
            if (ellipsesCount == 1) {
                if (isListEllipsis.get(0))
                    return Type.LAST_N_ITEMS;
                if (isListEllipsis.get(isListEllipsis.size() - 1))
                    return Type.FIRST_N_ITEMS;
            } else if (ellipsesCount == 2 && isListEllipsis.get(0) && isListEllipsis.get(isListEllipsis.size() - 1))
                return Type.CONTAINS;
            throw new SyntaxException("Invalid ellipsis", clauses.get(isListEllipsis.lastIndexOf(true))
                    .expression(null).getOperandPosition());
        }
        return Type.ALL_ITEMS;
    }

    @Override
    public String inspect() {
        if (type == Type.CONTAINS)
            return inputClauses.stream().map(clause -> clause.expression(InputNode.INSTANCE).inspect())
                    .collect(joining(", ", "[", "]"));
        return buildVerificationExpressions(0).stream().map(DALNode::inspect).collect(joining(", ", "[", "]"));
    }

    @Override
    protected boolean verify(Data actual, DALOperator.Equal operator, DALRuntimeContext context, DALNode actualNode) {
        return verify(context, actual);
    }

    @Override
    protected boolean verify(Data actual, DALOperator.Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        return verify(context, actual);
    }

    private boolean verify(DALRuntimeContext context, Data data) {
        if (!data.isList())
            throw new RuntimeException(format("Cannot compare %sand list", data.inspect()), getPositionBegin());
        data.setListComparator(comparator);
        return type == Type.CONTAINS ? verifyContainElement(context, data) : verifyEachElement(context, data);
    }

    private Boolean verifyEachElement(DALRuntimeContext context, Data data) {
        List<DALNode> expressions = getVerificationExpressions(data.getListFirstIndex());
        if (type == Type.ALL_ITEMS)
            assertListSize(expressions.size(), data.getListSize(), getPositionBegin());
        return context.newBlockScope(data, () -> assertElementExpressions(context, expressions));
    }

    private boolean verifyContainElement(DALRuntimeContext context, Data data) {
        //            TODO raise error when index list(expressionFactories == null)
        int elementIndex = 0;
        List<Clause<DALRuntimeContext, DALNode>> expected = trimFirstEllipsis();
        for (int clauseIndex = 0; clauseIndex < expected.size(); clauseIndex++) {
            Clause<DALRuntimeContext, DALNode> clause = expected.get(clauseIndex);
            while (!verifyContains(clauseIndex, context, data, elementIndex++, clause, data.getListFirstIndex())) ;
        }
        return true;
    }

    private List<Clause<DALRuntimeContext, DALNode>> trimFirstEllipsis() {
        return inputClauses.subList(1, inputClauses.size() - 1);
    }

    private boolean verifyContains(int clauseIndex, DALRuntimeContext context, Data data, int index,
                                   Clause<DALRuntimeContext, DALNode> clause, int firstIndex) {
        if (index == data.getListSize()) {
            int operandPosition = clause.expression(InputNode.INSTANCE).getOperandPosition();
            throw isTable ? new RowAssertionFailure(clauseIndex, new AssertionFailure("No such element",
                    operandPosition)) : new AssertionFailure("No such element", operandPosition);
        }
        try {
            return context.newBlockScope(data, () -> (boolean) clause.expression(new DALExpression(InputNode.INSTANCE,
                    new PropertyImplicit(), new SymbolNode(index + firstIndex, BRACKET))).evaluate(context));
        } catch (AssertionFailure ignore) {
            return false;
        }
    }

    private boolean assertElementExpressions(DALRuntimeContext context, List<DALNode> expressions) {
        if (isTable)
            for (int index = 0; index < expressions.size(); index++) {
                DALNode expression = expressions.get(index);
                try {
                    expression.evaluate(context);
                } catch (DalException dalException) {
                    throw new ElementAssertionFailure(index, dalException);
                }
            }
        else
            expressions.forEach(expression -> expression.evaluate(context));
        return true;
    }

    private boolean isListEllipsis(Clause<DALRuntimeContext, DALNode> clause) {
        return clause.expression(InputNode.INSTANCE) instanceof ListEllipsisNode;
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

    public static class NatureOrder extends ListScopeNode {

        @SuppressWarnings("unchecked")
        public NatureOrder(List<Clause<DALRuntimeContext, DALNode>> clauses) {
            super(clauses, false, (Comparator) naturalOrder());
        }

        @Override
        public String inspect() {
            return "+" + super.inspect();
        }
    }

    public static class ReverseOrder extends ListScopeNode {

        @SuppressWarnings("unchecked")
        public ReverseOrder(List<Clause<DALRuntimeContext, DALNode>> clauses) {
            super(clauses, false, (Comparator) reverseOrder());
        }

        @Override
        public String inspect() {
            return "-" + super.inspect();
        }
    }
}
