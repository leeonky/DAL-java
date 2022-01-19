package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

public interface ExpressionFactory<N extends Node<N>> {
    N fetch(TokenParser<N> tokenParser, N previous);

    default NodeFactory<N> toNode(N previous) {
        return parser -> fetch(parser, previous);
    }
}
