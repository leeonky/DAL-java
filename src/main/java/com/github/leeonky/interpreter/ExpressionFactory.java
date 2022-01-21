package com.github.leeonky.interpreter;

public interface ExpressionFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {
    N fetch(TokenParser<E, N, C> tokenParser, N previous);

    default NodeFactory<C, N, E> toNode(N previous) {
        return parser -> fetch(parser, previous);
    }
}
