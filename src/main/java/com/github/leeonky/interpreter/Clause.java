package com.github.leeonky.interpreter;

import java.util.function.Function;

public interface Clause<C extends RuntimeContext<C>, N extends Node<C, N>> {
    N makeExpression(N input);

    default Clause<C, N> map(Function<N, N> mapper) {
        return node -> mapper.apply(makeExpression(node));
    }
}
