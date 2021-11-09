package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.ExpressionClause;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class RowNode extends Node {
    private final List<Node> cells;
    private final Optional<Operator> operator;
    private final Optional<ExpressionClause> expressionClause;

    public RowNode(Optional<ExpressionClause> expressionClause, Optional<Operator> operator, List<Node> cells) {
        this.cells = cells;
        this.operator = operator;
        this.expressionClause = expressionClause;
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
        return expressionClause.map(clause -> clause.makeExpression(null).inspectClause() + " ").orElse("")
                + operator.map(o -> o.inspect("", "")).orElse("");
    }

    private boolean isRowWildcard() {
        return cells.size() == 1 && cells.get(0) instanceof WildcardNode;
    }

    private boolean isEllipsis() {
        return cells.size() == 1 && cells.get(0) instanceof ListEllipsisNode;
    }

    public ExpressionClause toExpressionClause(Operator operator) {
        return input -> isEllipsis() ? cells.get(0) :
                new Expression(expressionClause.map(c -> c.makeExpression(input)).orElse(input),
                        this.operator.orElse(operator), isRowWildcard() ? cells.get(0) : new ObjectNode(cells));
    }

    public boolean hasSchemaOrOperator() {
        return operator.isPresent() || expressionClause.isPresent();
    }

    public List<Node> getCells() {
        return cells;
    }

    public List<String> inspectCells() {
        return cells.stream().map(Node::inspectClause).collect(toList());
    }
}
