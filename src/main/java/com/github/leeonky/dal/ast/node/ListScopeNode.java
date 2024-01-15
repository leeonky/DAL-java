package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Factory;
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
import static com.github.leeonky.dal.runtime.ExpressionException.exception;
import static com.github.leeonky.dal.runtime.ExpressionException.opt1;
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
            List<Clause<DALNode>> usefulInputClauses = inputClauses;
            if (type == Type.FIRST_N_ITEMS)
                usefulInputClauses.remove(usefulInputClauses.size() - 1);
            if (type == Type.LAST_N_ITEMS) {
                int negativeIndex = -1;
                for (int i = usefulInputClauses.size() - 1; i >= 0; i--)
                    add(0, usefulInputClauses.get(i).expression(expression(INPUT_NODE, Factory.executable(Notations.EMPTY),
                            new SymbolNode(negativeIndex--, BRACKET))));
            } else {
                Zipped<Clause<DALNode>, Integer> zipped = zip(usefulInputClauses, list.indexes());
                zipped.forEachElement((clause, index) -> add(clause.expression(
                        expression(INPUT_NODE, Factory.executable(Notations.EMPTY), new SymbolNode(index, BRACKET)))));
                if (type == Type.ALL_ITEMS) {
                    if (zipped.hasLeft()) {
                        String message = format("Different list size\nExpected: <%d>\nActual: <%d>", usefulInputClauses.size(), zipped.index());
                        throw style == Style.ROW ? new DifferentCellSize(message, getPositionBegin())
                                : new AssertionFailure(message, getPositionBegin());
                    }
                    if (zipped.hasRight() && !list.infinite()) {
                        String message = format("Different list size\nExpected: <%d>\nActual: <%d>", usefulInputClauses.size(), list.size());
                        throw style == Style.ROW ? new DifferentCellSize(message, getPositionBegin())
                                : new AssertionFailure(message, getPositionBegin());
                    }
                }
            }
        }};
    }

    @Override
    protected ExpectationFactory toVerify(DALRuntimeContext context) {
        return (operator, actual) -> new ExpectationFactory.Expectation() {
            @Override
            public Data matches() {
                return equalTo();
            }

            @Override
            public Data equalTo() {
                try {
                    Data.DataList list = opt1(actual::list).sort(comparator);
                    return list.wrap().execute(() -> type == Type.CONTAINS ? verifyContainElement(context, list)
                            : verifyCorrespondingElement(context, getVerificationExpressions(list)));
                } catch (ListMappingElementAccessException e) {
                    throw exception(expression -> e.toDalError(expression.left().getOperandPosition()));
                }
            }

            @Override
            public ExpectationFactory.Type type() {
                return ExpectationFactory.Type.LIST;
            }
        };
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
    public String inspect() {
        if (type == Type.CONTAINS)
            return inputClauses.stream().map(clause -> clause.expression(INPUT_NODE).inspect())
                    .collect(joining(", ", "[", "]"));
        return buildVerificationExpressions().stream().map(DALNode::inspect).collect(joining(", ", "[", "]"));
    }

    private Data verifyContainElement(DALRuntimeContext context, Data.DataList list) {
        Iterator<Integer> iterator = list.indexes().iterator();
        List<Clause<DALNode>> expected = trimFirstEllipsis();
        Data result = context.wrap(null);
        for (int clauseIndex = 0; clauseIndex < expected.size(); clauseIndex++) {
            Clause<DALNode> clause = expected.get(clauseIndex);
            try {
                while (true) {
                    int elementIndex = getElementIndex(clause, iterator);
                    try {
                        clause.expression(expression(INPUT_NODE, Factory.executable(Notations.EMPTY),
                                new SymbolNode(elementIndex, BRACKET))).evaluate(context);
                        break;
                    } catch (AssertionFailure ignore) {
                    }
                }
            } catch (AssertionFailure exception) {
                throw style == Style.LIST ? exception : new RowAssertionFailure(clauseIndex, exception);
            }
        }
        return result;
    }

    private int getElementIndex(Clause<DALNode> clause, Iterator<Integer> iterator) {
        if (iterator.hasNext())
            return iterator.next();
        throw new AssertionFailure("No such element", clause.expression(INPUT_NODE).getOperandPosition());
    }

    private List<Clause<DALNode>> trimFirstEllipsis() {
        return inputClauses.subList(1, inputClauses.size() - 1);
    }

    private Data verifyCorrespondingElement(DALRuntimeContext context, List<DALNode> expressions) {
        Data result = context.wrap(null);
        if (style != Style.LIST)
            for (int index = 0; index < expressions.size(); index++)
                try {
                    result = expressions.get(index).evaluateData(context);
                } catch (DifferentCellSize differentCellSize) {
                    throw new RowAssertionFailure(index, differentCellSize);
                } catch (DalException dalException) {
                    if (style == Style.TABLE)
                        throw new ElementAssertionFailure(index, dalException);
                    throw dalException;
                }
        else {
            for (DALNode expression : expressions)
                result = expression.evaluateData(context);
        }
        return result;
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
