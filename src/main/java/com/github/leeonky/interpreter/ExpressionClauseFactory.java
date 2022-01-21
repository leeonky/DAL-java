package com.github.leeonky.interpreter;

public interface ExpressionClauseFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {
    ExpressionClause<C, N> fetch(TokenParser<E, N, C> tokenParser);

    default NodeFactory<C, N, E> input(N node) {
        return parser -> fetch(parser).makeExpression(node);
    }
}
