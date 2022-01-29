package com.github.leeonky.interpreter;

@Deprecated
public interface ExpressionFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> {
    N fetch(T tokenParser, N previous);

    default NodeFactory<C, N, E, O, T> toNode(N previous) {
        return parser -> fetch(parser, previous);
    }
}
