package com.github.leeonky.interpreter;

public interface OperatorFactory<N extends Node<N, C>, C extends RuntimeContext<C>> {
    Operator<N, C> fetch(TokenParser<N, C> tokenParser);
}
