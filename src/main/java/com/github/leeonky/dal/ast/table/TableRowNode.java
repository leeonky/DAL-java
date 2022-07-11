package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;

public class TableRowNode extends DALNode {
    private final List<Clause<DALRuntimeContext, DALNode>> cellClauses;
    private final TableHeadRow tableHeadRow;
    private final TableRowPrefixNode rowPrefix;

    public TableRowNode(DALNode prefix, DALNode cell, TableHeadRow tableHeadRow) {
        this(prefix, asList(n -> cell), tableHeadRow);
    }

    @Deprecated
    public TableRowNode(DALNode prefix, List<DALNode> cells) {
        rowPrefix = (TableRowPrefixNode) prefix;
        setPositionBegin(cells.get(cells.size() - 1).getOperandPosition());
        cellClauses = null;
        tableHeadRow = null;
    }

    public TableRowNode(DALNode prefix, List<Clause<DALRuntimeContext, DALNode>> clauses, TableHeadRow tableHeadRow) {
        rowPrefix = (TableRowPrefixNode) prefix;
        cellClauses = new ArrayList<>(clauses);
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
        return isRowWildcard() ? firstCell()
                : new ObjectScopeNode(getCells()).setPositionBegin(firstCell().getOperandPosition());
    }

    private DALNode firstCell() {
        return getCells().get(0);
    }

    private boolean isRowWildcard() {
        return cellClauses.size() >= 1 && firstCell() instanceof WildcardNode;
    }

    private boolean isEllipsis() {
        return cellClauses.size() >= 1 && firstCell() instanceof ListEllipsisNode;
    }

    public List<DALNode> getCells() {
        final @Deprecated List<DALNode> cells;
        cells = new ArrayList<>();
        for (int i = 0; i < cellClauses.size(); i++) {
            cells.add(cellClauses.get(i).expression(tableHeadRow.getHeader(i).property().parse(null)));
        }
        return cells;
    }

    public TableRowNode merge(TableRowNode rowNode) {
        return (TableRowNode) new TableRowNode(rowPrefix, new ArrayList<Clause<DALRuntimeContext, DALNode>>() {{
            addAll(cellClauses);
            addAll(rowNode.cellClauses);
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
}
