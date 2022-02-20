package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.ast.ObjectScopeNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static java.lang.String.join;

public class RowNode extends DALNode {
    private final Optional<Clause<DALRuntimeContext, DALNode>> rowSchema;
    private final Optional<DALOperator> rowOperator;
    private final List<DALNode> cells;

    public RowNode(Optional<Clause<DALRuntimeContext, DALNode>> rowSchemaClause,
                   Optional<DALOperator> rowOperator, List<DALNode> cells) {
        rowSchema = rowSchemaClause;
        this.rowOperator = rowOperator;
        this.cells = new ArrayList<>(cells);
    }

    @Override
    public String inspect() {
        return join(" ", new ArrayList<String>() {{
            rowSchema.map(clause -> clause.makeExpression(null).inspect()).ifPresent(this::add);
            rowOperator.map(dalOperator -> dalOperator.inspect("", "").trim()).ifPresent(this::add);
            add(TableNode.printLine(cells));
        }});
    }

    public Clause<DALRuntimeContext, DALNode> toExpressionClause(DALOperator operator) {
        return input -> new DALExpression(rowSchema.map(clause -> clause.makeExpression(input)).orElse(input),
                rowOperator.orElse(operator),
                new ObjectScopeNode(cells).setPositionBegin(cells.get(0).getOperandPosition()));
    }
}
