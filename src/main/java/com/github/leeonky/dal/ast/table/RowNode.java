package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.ast.ObjectScopeNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Clause;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class RowNode extends DALNode {
    private final Optional<DALOperator> rowOperator;
    private final List<DALNode> cells;

    public RowNode(Optional<DALOperator> rowOperator, List<DALNode> cells) {
        this.rowOperator = rowOperator;
        this.cells = new ArrayList<>(cells);
    }

    @Override
    public String inspect() {
        String row = TableNode.printLine(cells);
        return rowOperator.map(dalOperator -> dalOperator.inspect("", row)).orElse(row);
    }

    public Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode> toExpressionClause(DALOperator operator) {
        return input -> new DALExpression(input, rowOperator.orElse(operator), new ObjectScopeNode(cells)
                .setPositionBegin(cells.get(0).getOperandPosition()));
    }
}
