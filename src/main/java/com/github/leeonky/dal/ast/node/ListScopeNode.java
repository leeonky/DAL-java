package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Factory;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static com.github.leeonky.dal.ast.node.InputNode.INPUT_NODE;
import static com.github.leeonky.dal.ast.node.SymbolNode.Type.BRACKET;
import static java.lang.String.format;
import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.reverseOrder;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class ListScopeNode extends DALNode {
    private List<DALNode> verificationExpressions;
    private List<DALNode> inputExpressions;
    private List<Clause<DALNode>> inputClauses;
    private final Type type;
    private final Style style;
    private final Comparator<Object> comparator;

    public ListScopeNode(List<Clause<DALNode>> clauses, Comparator<Object> comparator, Style style) {
        type = guessType(clauses);
        inputClauses = clauses;
        this.comparator = comparator;
        this.style = style;
    }

    public ListScopeNode(List<DALNode> verificationExpressions, Type type, Comparator<Object> comparator, Style style) {
        this.verificationExpressions = inputExpressions = new ArrayList<>(verificationExpressions);
        this.type = type;
        this.comparator = comparator;
        this.style = style;
    }

    public ListScopeNode(List<Clause<DALNode>> clauses) {
        this(clauses, SortGroupNode.NOP_COMPARATOR, Style.LIST);
    }

    private void assertListSize(int expected, int actual, int position) {
        if (expected != actual) {
            String message = format("Different list size\nExpected: <%d>\nActual: <%d>", expected, actual);
            throw style == Style.ROW ? new DifferentCellSize(message, position) : new AssertionFailure(message, position);
        }
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
        return new DALExpression(INPUT_NODE, Factory.executable(Notations.EMPTY),
                new SymbolNode(type.indexOfNode(firstIndex, index, inputClauses.size()), BRACKET));
    }

    private Type guessType(List<Clause<DALNode>> clauses) {
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
            return inputClauses.stream().map(clause -> clause.expression(INPUT_NODE).inspect())
                    .collect(joining(", ", "[", "]"));
        return buildVerificationExpressions(0).stream().map(DALNode::inspect).collect(joining(", ", "[", "]"));
    }

    @Override
    protected boolean verify(Data actual, Equal operator, DALRuntimeContext context, DALNode actualNode) {
        return verify(context, actual);
    }

    @Override
    protected boolean verify(Data actual, Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        return verify(context, actual);
    }

    private boolean verify(DALRuntimeContext context, Data data) {
        data.requireList(getPositionBegin()).setListComparator(comparator);
        return type == Type.CONTAINS ? verifyContainElement(context, data) : verifyCorrespondingElement(context, data);
    }

    private Boolean verifyCorrespondingElement(DALRuntimeContext context, Data data) {
        List<DALNode> expressions = getVerificationExpressions(data.getListFirstIndex());
        if (type == Type.ALL_ITEMS)
            assertListSize(expressions.size(), data.getListSize(), getPositionBegin());
        return data.newBlockScope(() -> assertElementExpressions(context, expressions));
    }

    private boolean verifyContainElement(DALRuntimeContext context, Data data) {
        //            TODO raise error when index list(expressionFactories == null)
        int elementIndex = 0;
        List<Clause<DALNode>> expected = trimFirstEllipsis();
        for (int clauseIndex = 0; clauseIndex < expected.size(); clauseIndex++) {
            Clause<DALNode> clause = expected.get(clauseIndex);
            try {
                while (!isElementPassedVerification(context, clause, getElement(data, elementIndex++, clause))) ;
            } catch (AssertionFailure exception) {
                throw style == Style.LIST ? exception : new RowAssertionFailure(clauseIndex, exception);
            }
        }
        return true;
    }

    private boolean isElementPassedVerification(DALRuntimeContext context, Clause<DALNode> clause,
                                                Object element) {
        try {
            return (boolean) clause.expression(new ConstNode(element)).evaluate(context);
        } catch (AssertionFailure ignore) {
            return false;
        }
    }

    private Object getElement(Data data, int elementIndex, Clause<DALNode> clause) {
        if (elementIndex == data.getListSize())
            throw new AssertionFailure("No such element", clause.expression(INPUT_NODE).getOperandPosition());
        return data.getValueList().get(elementIndex);
    }

    private List<Clause<DALNode>> trimFirstEllipsis() {
        return inputClauses.subList(1, inputClauses.size() - 1);
    }

    private boolean assertElementExpressions(DALRuntimeContext context, List<DALNode> expressions) {
        if (style != Style.LIST)
            for (int index = 0; index < expressions.size(); index++)
                try {
                    expressions.get(index).evaluate(context);
                } catch (DifferentCellSize differentCellSize) {
                    throw new RowAssertionFailure(index, differentCellSize);
                } catch (DalException dalException) {
                    if (style == Style.TABLE)
                        throw new ElementAssertionFailure(index, dalException);
                    throw dalException;
                }
        else
            expressions.forEach(expression -> expression.evaluate(context));
        return true;
    }

    private boolean isListEllipsis(Clause<DALNode> clause) {
        return clause.expression(INPUT_NODE) instanceof ListEllipsisNode;
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

    public enum Style {
        LIST, TABLE, ROW
    }

    public static class NatureOrder extends ListScopeNode {

        @SuppressWarnings("unchecked")
        public NatureOrder(List<Clause<DALNode>> clauses) {
            super(clauses, (Comparator) naturalOrder(), Style.LIST);
        }

        @Override
        public String inspect() {
            return "+" + super.inspect();
        }
    }

    public static class ReverseOrder extends ListScopeNode {

        @SuppressWarnings("unchecked")
        public ReverseOrder(List<Clause<DALNode>> clauses) {
            super(clauses, (Comparator) reverseOrder(), Style.LIST);
        }

        @Override
        public String inspect() {
            return "-" + super.inspect();
        }
    }

    static class DifferentCellSize extends AssertionFailure {
        public DifferentCellSize(String format, int position) {
            super(format, position);
        }
    }
}
