package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.ExpressionClause;

import java.util.List;
import java.util.Optional;

public class RowNode extends Node {
    //    TODO to be private
    final List<Node> nodes;
    final Optional<Operator> operator;
    final Optional<ExpressionClause> expressionClause;

    public RowNode(Optional<ExpressionClause> expressionClause, Optional<Operator> operator, List<Node> nodes) {
        this.nodes = nodes;
        this.operator = operator;
        this.expressionClause = expressionClause;
    }

    @Override
    public String inspect() {
        return null;
    }

    private boolean isRowWildcard() {
        return nodes.size() == 1 && nodes.get(0) instanceof WildcardNode;
    }

    private boolean isEllipsis() {
        return nodes.size() == 1 && nodes.get(0) instanceof ListEllipsisNode;
    }

    //        TODO refactor
    public ExpressionClause toExpressionClause(Operator operator) {
        return input -> isEllipsis() ? nodes.get(0) :
                new Expression(expressionClause.map(c -> c.makeExpression(input)).orElse(input),
                        this.operator.orElse(operator), isRowWildcard() ? nodes.get(0) : new ObjectNode(nodes));
    }
}
