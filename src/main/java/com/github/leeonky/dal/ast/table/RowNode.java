package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static java.lang.String.join;
import static java.util.Arrays.asList;

public class RowNode extends DALNode {
    private final Optional<Integer> index;
    private final Optional<Clause<DALRuntimeContext, DALNode>> rowSchema;
    private final Optional<DALOperator> rowOperator;
    private final List<DALNode> cells;

    public RowNode(Optional<Integer> index, Optional<Clause<DALRuntimeContext, DALNode>> rowSchemaClause,
                   Optional<DALOperator> rowOperator, DALNode cell) {
        this(index, rowSchemaClause, rowOperator, asList(cell));
    }

    public RowNode(Optional<Integer> index, Optional<Clause<DALRuntimeContext, DALNode>> rowSchemaClause,
                   Optional<DALOperator> rowOperator, List<DALNode> cells) {
        this.index = index;
        rowSchema = rowSchemaClause;
        this.rowOperator = rowOperator;
        this.cells = new ArrayList<>(cells);
        setPositionBegin(cells.get(0).getOperandPosition());
    }

    @Override
    public String inspect() {
        return join(" ", new ArrayList<String>() {{
            index.map(Object::toString).ifPresent(this::add);
            rowSchema.map(clause -> clause.makeExpression(null).inspect()).ifPresent(this::add);
            rowOperator.map(dalOperator -> dalOperator.inspect("", "").trim()).ifPresent(this::add);
            add(TableNode.printLine(cells));
        }});
    }

    public Clause<DALRuntimeContext, DALNode> toExpressionClause(DALOperator operator) {
        return input -> isEllipsis() ? cells.get(0) : transformToExpression(operator,
                index.map(i -> (DALNode) new DALExpression(InputNode.INSTANCE, new DALOperator.PropertyImplicit(),
                        new SymbolNode(i, SymbolNode.Type.BRACKET))).orElse(input));
    }

    private DALExpression transformToExpression(DALOperator operator, DALNode input) {
        return new DALExpression(rowSchema.map(clause -> clause.makeExpression(input)).orElse(input),
                rowOperator.orElse(operator),
                isRowWildcard() ? cells.get(0) : new ObjectScopeNode(cells).setPositionBegin(cells.get(0).getOperandPosition()));
    }

    private boolean isRowWildcard() {
        return cells.size() >= 1 && cells.get(0) instanceof WildcardNode;
    }

    private boolean isEllipsis() {
        return cells.size() >= 1 && cells.get(0) instanceof ListEllipsisNode;
    }

    public boolean hasIndex() {
        return index.isPresent();
    }
}
