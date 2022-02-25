package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class RowNode extends DALNode {
    private final List<DALNode> cells;
    private final RowPrefixNode rowPrefix;

    public RowNode(DALNode prefix, DALNode cell) {
        this(prefix, Collections.singletonList(cell));
    }

    public RowNode(DALNode prefix, List<DALNode> cells) {
        rowPrefix = (RowPrefixNode) prefix;
        this.cells = new ArrayList<>(cells);
        setPositionBegin(cells.get(cells.size() - 1).getOperandPosition());
    }

    @Override
    public String inspect() {
        String prefix = rowPrefix.inspect();
        String data = TableNode.printLine(cells);
        return "\n" + (prefix.isEmpty() ? data : prefix + " " + data);
    }

    public Clause<DALRuntimeContext, DALNode> verificationClause(DALOperator operator) {
        return input -> isEllipsis() ? firstCell() : rowPrefix.indexAndSchema(input, operator, isRowWildcard() ?
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

    public boolean hasIndex() {
        return rowPrefix.hasIndex();
    }

    public List<DALNode> getCells() {
        return cells;
    }

    public boolean samePrefix(RowNode another) {
        return another.hasIndex() != hasIndex();
    }

    public RowNode merge(RowNode rowNode) {
        return (RowNode) new RowNode(rowPrefix, new ArrayList<DALNode>() {{
            addAll(cells);
            addAll(rowNode.cells);
        }}).setPositionBegin(getPositionBegin());
    }
}
