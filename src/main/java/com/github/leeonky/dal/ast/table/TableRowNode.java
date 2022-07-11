package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.List;

import static com.github.leeonky.interpreter.FunctionUtil.notAllowParallelReduce;
import static com.github.leeonky.interpreter.InterpreterException.Position.Type.CHAR;
import static java.util.Collections.singletonList;

public class TableRowNode extends DALNode {
    private final List<Clause<DALRuntimeContext, DALNode>> cells;
    private final TableHeadRow tableHeadRow;
    private final TableRowPrefixNode rowPrefix;

    public TableRowNode(DALNode prefix, DALNode cell, TableHeadRow tableHeadRow) {
        this(prefix, singletonList(n -> cell), tableHeadRow);
    }

    public TableRowNode(DALNode prefix, List<Clause<DALRuntimeContext, DALNode>> clauses, TableHeadRow tableHeadRow) {
        rowPrefix = (TableRowPrefixNode) prefix;
        cells = new ArrayList<>(clauses);
        this.tableHeadRow = tableHeadRow;
//        TODO move to clause
        setPositionBegin(clauses.get(clauses.size() - 1).expression(null).getOperandPosition());
    }

    @Override
    public String inspect() {
        String prefix = rowPrefix.inspect();
        String data = TableNode.printLine(getCells());
        return (prefix.isEmpty() ? data : prefix + " " + data);
    }

    public Clause<DALRuntimeContext, DALNode> constructVerificationClause(DALOperator operator, RowType rowType) {
        return input -> isEllipsis() ? firstCell() :
                rowPrefix.makeExpressionWithOptionalIndexAndSchema(rowType, input, operator, expectedRow());
    }

    private DALNode expectedRow() {
        if (isRowWildcard())
            return firstCell();
        if (tableHeadRow instanceof TableDefaultIndexHeadRow)
            return new ListScopeNode(cells, SortGroupNode.NOP_COMPARATOR, ListScopeNode.Style.ROW).setPositionBegin(getPositionBegin());
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

    @Deprecated
    public List<DALNode> getCells() {
        final List<DALNode> cells;
        cells = new ArrayList<>();
        for (int i = 0; i < this.cells.size(); i++)
            cells.add(this.cells.get(i).expression(tableHeadRow.getHeader(i).property().parse(null)));
        return cells;
    }

    public TableRowNode merge(TableRowNode rowNode) {
        return (TableRowNode) new TableRowNode(rowPrefix, new ArrayList<Clause<DALRuntimeContext, DALNode>>() {{
            addAll(cells);
            addAll(rowNode.cells);
        }}, tableHeadRow.merge(rowNode.tableHeadRow)).setPositionBegin(getPositionBegin());
    }

    public boolean isData() {
        return !isEllipsis();
    }

    public boolean specialRow() {
        return isEllipsis() || isRowWildcard();
    }

    public RowType mergeRowTypeBy(RowType rowType) {
        return rowType.merge(rowPrefix.resolveRowType());
    }

    void checkSize(int size) {
        if (!specialRow() && cells.size() != size)
//        TODO move to clause
            throw new SyntaxException("Different cell size", cells.get(cells.size() - 1).expression(null).getOperandPosition());
    }

    public DalException markPositionOnCells(DalException dalException) {
        return cells.stream().reduce(dalException, (e, cell) -> e.multiPosition(cell.expression(null).getPositionBegin(), CHAR),
                notAllowParallelReduce());
    }
}
