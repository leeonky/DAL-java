package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

public interface ExpressionFactory {
    Node fetchExpression(NodeParser nodeParser, Node previous);

    default ExpressionFactory combine(ExpressionFactory expressionFactory) {
        return (nodeParser, previous) -> {
            Node node = fetchExpression(nodeParser, previous);
            return node == null ? expressionFactory.fetchExpression(nodeParser, previous) : node;
        };
    }
}
