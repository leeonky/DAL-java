package com.github.leeonky.interpreter;

public interface OperatorFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {
    Operator<C, N> fetch(TokenParser<E, N, C> tokenParser);
}
