package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

public interface ExpressionFactory {
    Node fetch(TokenParser tokenParser, Node previous);

    default NodeFactory toNode(Node previous) {
        return parser -> fetch(parser, previous);
    }
}
