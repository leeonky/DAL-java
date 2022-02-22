package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class RowNode extends DALNode {
    private final List<DALNode> cells;
    private final RowPrefixNode rowPrefixNode;

    public RowNode(DALNode prefix, DALNode cell) {
        this(prefix, Collections.singletonList(cell));
    }

    public RowNode(DALNode prefix, List<DALNode> cells) {
        rowPrefixNode = (RowPrefixNode) prefix;
        this.cells = new ArrayList<>(cells);
        setPositionBegin(cells.get(0).getOperandPosition());
    }

    @Override
    public String inspect() {
        String prefix = rowPrefixNode.inspect();
        String data = TableNode.printLine(cells);
        return prefix.isEmpty() ? data : prefix + " " + data;
    }

    public Clause<DALRuntimeContext, DALNode> toExpressionClause(DALOperator operator) {
        return input -> isEllipsis() ? firstCell() : rowPrefixNode.transformToExpression(input, operator,
                isRowWildcard() ? firstCell() : new ObjectScopeNode(cells).setPositionBegin(firstCell().getOperandPosition())
        );
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

    @Deprecated
    public boolean hasIndex() {
        return rowPrefixNode.hasIndex();
    }
}
