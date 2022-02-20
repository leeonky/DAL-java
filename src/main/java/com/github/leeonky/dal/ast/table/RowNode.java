package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.ast.ObjectScopeNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Clause;

import java.util.ArrayList;
import java.util.List;

public class RowNode extends DALNode {
    private final List<DALNode> cells;

    public RowNode(List<DALNode> cells) {
        this.cells = new ArrayList<>(cells);
    }

    @Override
    public String inspect() {
        return TableNode.printLine(cells);
    }

    public Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode> toExpressionClause(DALOperator operator) {
        return input -> new DALExpression(input, operator, new ObjectScopeNode(cells)
                .setPositionBegin(cells.get(0).getOperandPosition()));
    }
}
