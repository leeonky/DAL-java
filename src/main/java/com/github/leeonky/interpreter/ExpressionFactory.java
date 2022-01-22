package com.github.leeonky.interpreter;

public interface ExpressionFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>> {
    N fetch(TokenParser<C, N, E, O> tokenParser, N previous);

    default NodeFactory<C, N, E, O> toNode(N previous) {
        return parser -> fetch(parser, previous);
    }
}
