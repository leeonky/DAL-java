package com.github.leeonky.dal.ast;

import java.util.Optional;

import static java.util.Optional.ofNullable;

public interface ExpressionFactory {
    Node fetchExpression(NodeParser nodeParser, Node previous);

    default ExpressionFactory combine(ExpressionFactory expressionFactory) {
        return (nodeParser, previous) -> fetchExpressionOptional(nodeParser, previous)
                .orElseGet(() -> expressionFactory.fetchExpression(nodeParser, previous));
    }

    default NodeFactory inThis() {
        return nodeParser -> fetchExpression(nodeParser, InputNode.INSTANCE);
    }

    default Optional<Node> fetchExpressionOptional(NodeParser nodeParser, Node previous) {
        return ofNullable(fetchExpression(nodeParser, previous));
    }
}
