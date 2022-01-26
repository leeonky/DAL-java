package com.github.leeonky.interpreter;

public interface OperatorFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> {
    O fetch(T tokenParser);
}