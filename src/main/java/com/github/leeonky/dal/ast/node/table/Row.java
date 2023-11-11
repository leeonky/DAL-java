package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.*;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.List;

import static com.github.leeonky.dal.ast.node.InputNode.INPUT_NODE;
import static com.github.leeonky.dal.ast.node.ListScopeNode.Style.ROW;
import static com.github.leeonky.dal.ast.node.SortGroupNode.NOP_COMPARATOR;
import static com.github.leeonky.dal.ast.node.TableNode.printLine;
import static com.github.leeonky.interpreter.InterpreterException.Position.Type.COLUMN;
import static com.github.leeonky.util.function.Extension.notAllowParallelReduce;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;

public class Row extends DALNode {
    private final RowHeader rowHeader;
    private final List<Clause<DALNode>> cells;
    private final ColumnHeaderRow columnHeaderRow;

    public Row(DALNode rowHeader, DALNode cell, ColumnHeaderRow columnHeaderRow) {
        this(rowHeader, singletonList(n -> cell), columnHeaderRow);
    }

    public Row(DALNode rowHeader, List<Clause<DALNode>> clauses, ColumnHeaderRow columnHeaderRow) {
        this.rowHeader = (RowHeader) rowHeader;
        cells = new ArrayList<>(clauses);
        this.columnHeaderRow = columnHeaderRow;
        setPositionBegin(clauses.get(clauses.size() - 1).getOperandPosition(INPUT_NODE));
    }

    @Override
    public String inspect() {
        String header = rowHeader.inspect();
        String data = printLine(cells.stream().map(clause -> clause.expression(INPUT_NODE)).collect(toList()));
        return (header.isEmpty() ? data : header + " " + data);
    }

    public Clause<DALNode> constructVerificationClause(DALOperator operator, RowType rowType) {
        return input -> isEllipsis() ? firstCell() :
                rowHeader.makeExpressionWithOptionalIndexAndSchema(rowType, input, operator, expectedRow());
    }

    private DALNode expectedRow() {
        if (isRowWildcard())
            return firstCell();
        if (columnHeaderRow instanceof DefaultIndexColumnHeaderRow)
            return new ListScopeNode(cells, NOP_COMPARATOR, ROW).setPositionBegin(getPositionBegin());
        return new ObjectScopeNode(getCells()).setPositionBegin(getPositionBegin());
    }

    private DALNode firstCell() {
        return cells.get(0).expression(null);
    }

    private boolean isRowWildcard() {
        return cells.size() >= 1 && firstCell() instanceof WildcardNode;
    }

    private boolean isEllipsis() {
        return cells.size() >= 1 && firstCell() instanceof ListEllipsisNode;
    }

    private List<DALNode> getCells() {
        return new ArrayList<DALNode>() {{
            for (int i = 0; i < cells.size(); i++)
                add(cells.get(i).expression(columnHeaderRow.getHeader(i).property()));
        }};
    }

    public Row merge(Row rowNode) {
        return (Row) new Row(rowHeader, new ArrayList<Clause<DALNode>>() {{
            addAll(cells);
            addAll(rowNode.cells);
        }}, columnHeaderRow.merge(rowNode.columnHeaderRow)).setPositionBegin(getPositionBegin());
    }

    public boolean isData() {
        return !isEllipsis();
    }

    public boolean specialRow() {
        return isEllipsis() || isRowWildcard();
    }

    public RowType mergeRowTypeBy(RowType rowType) {
        return rowType.merge(rowHeader.resolveRowType());
    }

    public void checkSize(int size) {
        if (!specialRow() && cells.size() != size)
            throw new SyntaxException("Different cell size", cells.get(cells.size() - 1).getOperandPosition(INPUT_NODE));
    }


    public DalException markPositionOnCells(DalException dalException) {
        rowHeader.position().ifPresent(position -> dalException.multiPosition(position, COLUMN));
        return cells.stream().reduce(dalException, (e, cell) ->
                e.multiPosition(cell.expression(null).getPositionBegin(), COLUMN), notAllowParallelReduce());
    }
}
