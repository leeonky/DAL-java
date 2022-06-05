package com.github.leeonky.interpreter;

public interface ExpressionFactory<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>> {
    E create(N left, O operator, N right);
}
