package com.github.leeonky.interpreter;

public interface ExpressionConstructor<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>> {
    E newInstance(N node1, O operator, N node2);
}
