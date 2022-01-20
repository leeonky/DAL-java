package com.github.leeonky.interpreter;

import java.util.function.Function;

public interface ExpressionClause<N extends Node<N, C>, C extends RuntimeContext<C>> {
    N makeExpression(N input);

    default ExpressionClause<N, C> map(Function<N, N> mapper) {
        return node -> mapper.apply(makeExpression(node));
    }
}
