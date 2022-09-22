package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.*;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.List;

import static com.github.leeonky.dal.ast.node.InputNode.INPUT_NODE;
import static com.github.leeonky.dal.ast.node.ListScopeNode.Style.ROW;
import static com.github.leeonky.dal.ast.node.SortGroupNode.NOP_COMPARATOR;
import static com.github.leeonky.dal.ast.node.TableNode.printLine;
import static com.github.leeonky.interpreter.InterpreterException.Position.Type.CHAR;
import static com.github.leeonky.util.function.Extension.notAllowParallelReduce;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;

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
        setPositionBegin(clauses.get(clauses.size() - 1).getOperandPosition(INPUT_NODE));
    }

    @Override
    public String inspect() {
        String prefix = rowPrefix.inspect();
        String data = printLine(cells.stream().map(clause -> clause.expression(INPUT_NODE)).collect(toList()));
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
                add(cells.get(i).expression(tableHeadRow.getHeader(i).property()));
        }};
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

    public void checkSize(int size) {
        if (!specialRow() && cells.size() != size)
            throw new SyntaxException("Different cell size", cells.get(cells.size() - 1).getOperandPosition(INPUT_NODE));
    }


    public DalException markPositionOnCells(DalException dalException) {
        return cells.stream().reduce(dalException, (e, cell) ->
                e.multiPosition(cell.expression(null).getPositionBegin(), CHAR), notAllowParallelReduce());
    }
}
