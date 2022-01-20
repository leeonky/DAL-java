package com.github.leeonky.interpreter;

public interface ExpressionFactory<N extends Node<N, C>, C extends RuntimeContext<C>> {
    N fetch(TokenParser<N, C> tokenParser, N previous);

    default NodeFactory<N, C> toNode(N previous) {
        return parser -> fetch(parser, previous);
    }
}
