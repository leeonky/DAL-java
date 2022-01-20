package com.github.leeonky.interpreter;

public interface ExpressionConstructor<N extends Node<N, C>, C extends RuntimeContext<C>> {
    N newInstance(N node1, Operator<N, C> operator, N node2);
}
