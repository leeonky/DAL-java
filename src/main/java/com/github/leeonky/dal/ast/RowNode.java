package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.ExpressionClause;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class RowNode extends Node {
    private final List<Node> cells;
    private final Optional<Integer> index;
    private final Optional<Operator> operator;
    private final Optional<ExpressionClause> schemaClause;

    public RowNode(Optional<Integer> index, Optional<ExpressionClause> schemaClause, Optional<Operator> operator,
                   List<Node> cells) {
        this.cells = cells;
        this.operator = operator;
        this.schemaClause = schemaClause;
        this.index = index;
    }

    public static String printTableRow(Stream<String> stream) {
        return stream.collect(joining(" | ", "| ", " |"));
    }

    public static String printTableRow(Collection<String> collection) {
        return printTableRow(collection.stream());
    }

    @Override
    public String inspect() {
        return inspectSchemaAndOperator() + cells.stream().map(Node::inspectClause)
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

    public ExpressionClause toExpressionClause(Operator operator) {
        return input -> isEllipsis() ? cells.get(0) : transformRowToExpression(operator,
                index.<Node>map(i -> new PropertyNode(InputNode.INSTANCE, i, BRACKET)).orElse(input));
    }

    private Expression transformRowToExpression(Operator operator, Node inputElement) {
        return new Expression(schemaClause.map(c -> c.makeExpression(inputElement)).orElse(inputElement),
                this.operator.orElse(operator), isRowWildcard() ? cells.get(0)
                : new ObjectNode(cells).setPositionBegin(cells.get(0).getOperandPosition()));
    }

    public boolean hasSchemaOrOperator() {
        return operator.isPresent() || schemaClause.isPresent() || hasIndex();
    }

    public List<Node> getCells() {
        return cells;
    }

    public List<String> inspectCells() {
        return cells.stream().map(Node::inspectClause).collect(toList());
    }

    public boolean hasIndex() {
        return index.isPresent();
    }
}
