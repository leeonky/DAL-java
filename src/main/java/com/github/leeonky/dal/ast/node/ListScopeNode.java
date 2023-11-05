package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Factory;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.SyntaxException;
import com.github.leeonky.util.Zipped;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import static com.github.leeonky.dal.ast.node.DALExpression.expression;
import static com.github.leeonky.dal.ast.node.InputNode.INPUT_NODE;
import static com.github.leeonky.dal.ast.node.SortGroupNode.NOP_COMPARATOR;
import static com.github.leeonky.dal.ast.node.SymbolNode.Type.BRACKET;
import static com.github.leeonky.util.Zipped.zip;
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
    private final Comparator<Data> comparator;

    public ListScopeNode(List<Clause<DALNode>> clauses, Comparator<Data> comparator, Style style) {
        type = guessType(clauses);
        inputClauses = clauses;
        this.comparator = comparator;
        this.style = style;
    }

    public ListScopeNode(List<DALNode> verificationExpressions, Type type, Comparator<Data> comparator, Style style) {
        this.verificationExpressions = inputExpressions = new ArrayList<>(verificationExpressions);
        this.type = type;
        this.comparator = comparator;
        this.style = style;
    }

    public ListScopeNode(List<Clause<DALNode>> clauses) {
        this(clauses, NOP_COMPARATOR, Style.LIST);
    }

    private List<DALNode> getVerificationExpressions(Data.DataList list) {
        return verificationExpressions != null ? verificationExpressions : buildVerificationExpressions(list)
                .stream().filter(node -> !(node instanceof ListEllipsisNode)).collect(toList());
    }

    private List<DALNode> buildVerificationExpressions(Data.DataList list) {
        if (inputExpressions != null)
            return inputExpressions;
        return new ArrayList<DALNode>() {{
            if (type == Type.LAST_N_ITEMS) {
                int negativeIndex = -1;
                for (int i = inputClauses.size() - 1; i >= 0; i--)
                    add(0, inputClauses.get(i).expression(expression(INPUT_NODE, Factory.executable(Notations.EMPTY),
                            new SymbolNode(negativeIndex--, BRACKET))));
            } else {
                Zipped<Integer, Clause<DALNode>> zipped = zip(list.indexes(), inputClauses);
                zipped.forEachElement((index, clause) -> add(clause.expression(
                        expression(INPUT_NODE, Factory.executable(Notations.EMPTY), new SymbolNode(index, BRACKET)))));
                if (type == Type.ALL_ITEMS) {
                    if (zipped.hasRight()) {
                        String message = format("Different list size\nExpected: <%d>\nActual: <%d>", inputClauses.size(), zipped.index());
                        throw style == Style.ROW ? new DifferentCellSize(message, getPositionBegin())
                                : new AssertionFailure(message, getPositionBegin());
                    }
                    if (zipped.hasLeft() && !list.infinite()) {
                        String message = format("Different list size\nExpected: <%d>\nActual: <%d>", inputClauses.size(), list.size());
                        throw style == Style.ROW ? new DifferentCellSize(message, getPositionBegin())
                                : new AssertionFailure(message, getPositionBegin());
                    }
                }
            }
        }};
    }

    //    TODO tobe refactored
    private List<DALNode> buildVerificationExpressions() {
        if (inputExpressions != null)
            return inputExpressions;
        return new ArrayList<DALNode>() {{
            if (type == Type.LAST_N_ITEMS) {
                int negativeIndex = -1;
                for (int i = inputClauses.size() - 1; i >= 0; i--) {
                    add(0, inputClauses.get(i).expression(expression(INPUT_NODE, Factory.executable(Notations.EMPTY),
                            new SymbolNode(negativeIndex--, BRACKET))));
                }
            } else {
                for (int i = 0; i < inputClauses.size(); i++)
                    add(inputClauses.get(i).expression(expression(INPUT_NODE, Factory.executable(Notations.EMPTY),
                            new SymbolNode(i, BRACKET))));
            }
        }};
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
    public Object evaluate(DALRuntimeContext context) {
        return NodeType.LIST_SCOPE;
    }

    @Override
    public String inspect() {
        if (type == Type.CONTAINS)
            return inputClauses.stream().map(clause -> clause.expression(INPUT_NODE).inspect())
                    .collect(joining(", ", "[", "]"));
        return buildVerificationExpressions().stream().map(DALNode::inspect).collect(joining(", ", "[", "]"));
    }

    @Override
    public Data verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        Data data = verify(context, actualNode);
        Data placeholder = evaluateData(context);
        return checkerVerify(context.fetchMatchingChecker(placeholder, data), placeholder, data, context);
    }

    @Override
    public Data verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        Data data = verify(context, actualNode);
        Data placeholder = evaluateData(context);
        return checkerVerify(context.fetchEqualsChecker(placeholder, data), placeholder, data, context);
    }

    private Data verify(DALRuntimeContext context, DALNode node) {
        Data data = node.evaluateData(context);
        try {
            Data.DataList list = data.list(node.getOperandPosition()).sort(comparator);
            return list.wrap().execute(() -> {
                if (type == Type.CONTAINS)
                    verifyContainElement(context, list);
                else
                    verifyCorrespondingElement(context, getVerificationExpressions(list));
                return data;
            });
        } catch (ListMappingElementAccessException e) {
            throw e.toDalError(node.getOperandPosition());
        }
    }

    private void verifyContainElement(DALRuntimeContext context, Data.DataList list) {
        Iterator<Integer> iterator = list.indexes().iterator();
        List<Clause<DALNode>> expected = trimFirstEllipsis();
        for (int clauseIndex = 0; clauseIndex < expected.size(); clauseIndex++) {
            Clause<DALNode> clause = expected.get(clauseIndex);
            try {
                while (!isElementPassedVerification(context, clause, getElementIndex(clause, iterator))) ;
            } catch (AssertionFailure exception) {
                throw style == Style.LIST ? exception : new RowAssertionFailure(clauseIndex, exception);
            }
        }
    }

    private int getElementIndex(Clause<DALNode> clause, Iterator<Integer> iterator) {
        if (iterator.hasNext())
            return iterator.next();
        throw new AssertionFailure("No such element", clause.expression(INPUT_NODE).getOperandPosition());
    }

    private boolean isElementPassedVerification(DALRuntimeContext context, Clause<DALNode> clause, int index) {
        try {
            clause.expression(expression(INPUT_NODE, Factory.executable(Notations.EMPTY),
                    new SymbolNode(index, BRACKET))).evaluate(context);
            return true;
        } catch (AssertionFailure ignore) {
            return false;
        }
    }

    private List<Clause<DALNode>> trimFirstEllipsis() {
        return inputClauses.subList(1, inputClauses.size() - 1);
    }

    private void verifyCorrespondingElement(DALRuntimeContext context, List<DALNode> expressions) {
        if (style != Style.LIST)
            for (int index = 0; index < expressions.size(); index++)
                try {
                    expressions.get(index).evaluate(context);
                } catch (DifferentCellSize differentCellSize) {
                    throw new RowAssertionFailure(index, differentCellSize);
                } catch (DALException dalException) {
                    if (style == Style.TABLE)
                        throw new ElementAssertionFailure(index, dalException);
                    throw dalException;
                }
        else
            expressions.forEach(expression -> expression.evaluate(context));
    }

    private boolean isListEllipsis(Clause<DALNode> clause) {
        return clause.expression(INPUT_NODE) instanceof ListEllipsisNode;
    }

    public enum Type {
        ALL_ITEMS, FIRST_N_ITEMS, LAST_N_ITEMS, CONTAINS
    }

    public enum Style {
        LIST, TABLE, ROW
    }

    public static class NatureOrder extends ListScopeNode {

        @SuppressWarnings("unchecked")
        public NatureOrder(List<Clause<DALNode>> clauses) {
            super(clauses, Comparator.comparing(Data::instance, (Comparator) naturalOrder()), Style.LIST);
        }

        @Override
        public String inspect() {
            return "+" + super.inspect();
        }
    }

    public static class ReverseOrder extends ListScopeNode {

        @SuppressWarnings("unchecked")
        public ReverseOrder(List<Clause<DALNode>> clauses) {
            super(clauses, Comparator.comparing(Data::instance, (Comparator) reverseOrder()), Style.LIST);
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
