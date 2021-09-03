package com.github.leeonky.dal.ast;

import static java.util.Optional.ofNullable;

public interface ExpressionFactory {
    Node fetchExpression(NodeParser nodeParser, Node previous);

    default ExpressionFactory combine(ExpressionFactory expressionFactory) {
        return (nodeParser, previous) -> ofNullable(fetchExpression(nodeParser, previous))
                .orElseGet(() -> expressionFactory.fetchExpression(nodeParser, previous));
    }

    default NodeFactory inThis() {
        return nodeParser -> fetchExpression(nodeParser, InputNode.INSTANCE);
    }
}
