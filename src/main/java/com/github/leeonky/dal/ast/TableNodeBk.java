package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.HeaderNodeBk.bySequence;
import static com.github.leeonky.dal.ast.RowNode.printTableRow;
import static com.github.leeonky.interpreter.FunctionUtil.transpose;
import static com.github.leeonky.interpreter.FunctionUtil.zip;
import static com.github.leeonky.interpreter.InterpreterException.Position.Type.CHAR;
import static com.github.leeonky.interpreter.InterpreterException.Position.Type.LINE;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

@Deprecated
public class TableNodeBk extends DALNode {
    private final List<HeaderNodeBk> headers;
    private final List<RowNode> rows;
    private final Type type;
    private final boolean hasRowIndex;

    public TableNodeBk(List<HeaderNodeBk> headers, List<RowNode> row) {
        this(headers, row, Type.NORMAL);
    }

    public TableNodeBk(List<HeaderNodeBk> headers, List<RowNode> rows, Type type) {
        this.headers = new ArrayList<>(headers);
        this.rows = new ArrayList<>(rows);
        this.type = type;
        hasRowIndex = (!rows.isEmpty()) && rows.get(0).hasIndex();
        checkRowIndex(rows);
    }

    private void checkRowIndex(List<RowNode> rows) {
        rows.stream().skip(1).filter(rowNode -> hasRowIndex ^ rowNode.hasIndex()).findAny()
                .ifPresent(row -> type.raiseInvalidRowIndex(rows.get(0), row));
    }

    public List<HeaderNodeBk> getHeaders() {
        return headers;
    }

    public List<RowNode> getRows() {
        return rows;
    }

    @Override
    public String inspect() {
        return type.inspect(headers, rows);
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Equal operator, DALRuntimeContext context) {
        return judgeRows(actualNode, operator, context);
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Matcher operator, DALRuntimeContext context) {
        return judgeRows(actualNode, operator, context);
    }

    private boolean judgeRows(DALNode actualNode, DALOperator operator, DALRuntimeContext context) {
        try {
            return transformToListNode(operator).judgeAll(context, actualNode.evaluateData(context)
                    .setListComparator(collectComparator(context)));
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw type.toDalException(elementAssertionFailure, this);
        }
    }

    private ListScopeNode transformToListNode(DALOperator operator) {
        Stream<Clause<DALRuntimeContext, DALNode>> rowExpressionClauses = rows.stream().map(rowNode -> rowNode.toExpressionClause(operator));
        return hasRowIndex ? new ListScopeNode(rowExpressionClauses.map(rowNode -> rowNode.makeExpression(null))
                .collect(toList()), true, ListScopeNode.Type.FIRST_N_ITEMS)
                : new ListScopeNode(rowExpressionClauses.collect(toList()), true);
    }

    private Comparator<Object> collectComparator(DALRuntimeContext context) {
        return headers.stream().sorted(bySequence())
                .map(headerNode -> headerNode.getListComparator(context))
                .reduce(Comparator::thenComparing)
                .orElse(SortSequenceNode.NOP_COMPARATOR);
    }

    public enum Type {
        NORMAL, TRANSPOSED {
            @Override
            public DalException toDalException(ElementAssertionFailure elementAssertionFailure, TableNodeBk tableNode) {
                return elementAssertionFailure.columnPositionException(tableNode);
            }

            @Override
            public String inspect(List<HeaderNodeBk> headers, List<RowNode> rows) {
                String tableContent = zip(headers.stream().map(HeaderNodeBk::inspect).collect(toList()).stream(),
                        inspectCells(rows, headers.size()).stream(), this::mergeHeaderAndCells)
                        .map(RowNode::printTableRow).collect(joining("\n"));
                return rows.stream().anyMatch(RowNode::hasSchemaOrOperator) ?
                        String.format("| >> %s\n%s", printTableRow(rows.stream().map(rowNode ->
                                rowNode.inspectSchemaAndOperator().trim())), tableContent) : ">>" + tableContent;
            }

            @Override
            public void raiseInvalidRowIndex(RowNode firstRow, RowNode invalidRow) {
                throw new SyntaxException("Row index should be consistent",
                        firstRow.getCells().get(0).getPositionBegin(), CHAR) {{
                    firstRow.getCells().stream().skip(1).forEach(cell ->
                            multiPosition(cell.getPositionBegin(), CHAR));
                    invalidRow.getCells().forEach(cell ->
                            multiPosition(cell.getPositionBegin(), CHAR));
                }};
            }

            private ArrayList<String> mergeHeaderAndCells(String h, List<String> cells) {
                return new ArrayList<String>() {{
                    add(h);
                    addAll(cells);
                }};
            }

            private List<List<String>> inspectCells(List<RowNode> rows, int headerCount) {
                List<List<String>> rowCells = transpose(rows.stream().map(RowNode::inspectCells).collect(toList()));
                return rowCells.isEmpty() ? Collections.nCopies(headerCount, emptyList()) : rowCells;
            }
        };

        public DalException toDalException(ElementAssertionFailure elementAssertionFailure, TableNodeBk tableNode) {
            return elementAssertionFailure.linePositionException();
        }

        public String inspect(List<HeaderNodeBk> headers, List<RowNode> rows) {
            return String.join("\n", new ArrayList<String>() {{
                add(printTableRow(headers.stream().map(HeaderNodeBk::inspect)));
                rows.stream().map(RowNode::inspect).forEach(this::add);
            }});
        }

        public void raiseInvalidRowIndex(RowNode firstRow, RowNode invalidRow) {
            throw new SyntaxException("Row index should be consistent", invalidRow.getPositionBegin(), LINE)
                    .multiPosition(firstRow.getPositionBegin(), LINE);
        }
    }
}
