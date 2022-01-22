package com.github.leeonky.interpreter;

public interface OperatorFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>, O extends Operator<C, N, O>> {
    O fetch(TokenParser<C, N, E, O> tokenParser);
}
