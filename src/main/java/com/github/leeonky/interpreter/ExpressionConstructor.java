package com.github.leeonky.interpreter;

public interface ExpressionConstructor<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {
    E newInstance(N node1, Operator<C, N> operator, N node2);
}
