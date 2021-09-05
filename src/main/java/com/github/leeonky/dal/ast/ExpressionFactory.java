package com.github.leeonky.dal.ast;

public interface ExpressionFactory {
    Node fetchExpression(NodeParser nodeParser, Node previous);

    default NodeFactory inThis() {
        return nodeParser -> fetchExpression(nodeParser, InputNode.INSTANCE);
    }
}
