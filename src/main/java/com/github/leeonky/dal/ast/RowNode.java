package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.ExpressionClause;
import com.github.leeonky.interpreter.Operator;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class RowNode extends DALNode {
    private final List<DALNode> cells;
    private final Optional<Integer> index;
    private final Optional<Operator<DALNode, DALRuntimeContext>> operator;
    private final Optional<ExpressionClause<DALNode, DALRuntimeContext>> schemaClause;

    public RowNode(Optional<Integer> index, Optional<ExpressionClause<DALNode, DALRuntimeContext>> schemaClause, Optional<Operator<DALNode, DALRuntimeContext>> operator,
                   List<DALNode> cells) {
        this.cells = cells;
        this.operator = operator;
        this.schemaClause = schemaClause;
        this.index = index;
        setPositionBegin(cells.get(0).getOperandPosition());
    }

    public static String printTableRow(Stream<String> stream) {
        return stream.collect(joining(" | ", "| ", " |"));
    }

    public static String printTableRow(Collection<String> collection) {
        return printTableRow(collection.stream());
    }

    @Override
    public String inspect() {
        return inspectSchemaAndOperator() + cells.stream().map(DALNode::inspectClause)
                .collect(Collectors.joining(" | ", "| ", " |"));
    }

    public String inspectSchemaAndOperator() {
        return index.map(i -> i + " ").orElse("")
                + schemaClause.map(clause -> clause.makeExpression(null).inspectClause() + " ").orElse("")
                + operator.map(o -> o.inspect("", "")).orElse("");
    }

    private boolean isRowWildcard() {
        return cells.size() >= 1 && cells.get(0) instanceof WildcardNode;
    }

    private boolean isEllipsis() {
        return cells.size() >= 1 && cells.get(0) instanceof ListEllipsisNode;
    }

    public ExpressionClause<DALNode, DALRuntimeContext> toExpressionClause(Operator<DALNode, DALRuntimeContext> operator) {
        return input -> isEllipsis() ? cells.get(0) : transformRowToExpression(operator,
                index.<DALNode>map(i -> new PropertyNode(InputNode.INSTANCE, i, BRACKET)).orElse(input));
    }

    private Expression transformRowToExpression(Operator<DALNode, DALRuntimeContext> operator, DALNode inputElement) {
        return new Expression(schemaClause.map(c -> c.makeExpression(inputElement)).orElse(inputElement),
                this.operator.orElse(operator), isRowWildcard() ? cells.get(0)
                : new ObjectNode(cells).setPositionBegin(cells.get(0).getOperandPosition()));
    }

    public boolean hasSchemaOrOperator() {
        return operator.isPresent() || schemaClause.isPresent() || hasIndex();
    }

    public List<DALNode> getCells() {
        return cells;
    }

    public List<String> inspectCells() {
        return cells.stream().map(DALNode::inspectClause).collect(toList());
    }

    public boolean hasIndex() {
        return index.isPresent();
    }
}
