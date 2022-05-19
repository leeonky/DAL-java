package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TableRowNode extends DALNode {
    private final List<DALNode> cells;
    private final TableRowPrefixNode rowPrefix;

    public TableRowNode(DALNode prefix, DALNode cell) {
        this(prefix, Collections.singletonList(cell));
    }

    public TableRowNode(DALNode prefix, List<DALNode> cells) {
        rowPrefix = (TableRowPrefixNode) prefix;
        this.cells = new ArrayList<>(cells);
        setPositionBegin(cells.get(cells.size() - 1).getOperandPosition());
    }

    @Override
    public String inspect() {
        String prefix = rowPrefix.inspect();
        String data = TableNode.printLine(cells);
        return "\n" + (prefix.isEmpty() ? data : prefix + " " + data);
    }

    public Clause<DALRuntimeContext, DALNode> constructVerificationClause(DALOperator operator, RowType rowType) {
        return input -> isEllipsis() ? firstCell() : rowPrefix.indexAndSchema(rowType, input, operator, isRowWildcard() ?
                firstCell() : new ObjectScopeNode(cells).setPositionBegin(firstCell().getOperandPosition()));
    }

    private DALNode firstCell() {
        return cells.get(0);
    }

    private boolean isRowWildcard() {
        return cells.size() >= 1 && firstCell() instanceof WildcardNode;
    }

    private boolean isEllipsis() {
        return cells.size() >= 1 && firstCell() instanceof ListEllipsisNode;
    }

    public List<DALNode> getCells() {
        return cells;
    }

    public TableRowNode merge(TableRowNode rowNode) {
        return (TableRowNode) new TableRowNode(rowPrefix, new ArrayList<DALNode>() {{
            addAll(cells);
            addAll(rowNode.cells);
        }}).setPositionBegin(getPositionBegin());
    }

    public boolean isData() {
        return !isEllipsis();
    }

    public boolean specialRow() {
        return isEllipsis() || isRowWildcard();
    }

    public RowType combineRowKey(RowType rowType) {
        return rowType.merge(rowPrefix.getRowKeyType());
    }
}
