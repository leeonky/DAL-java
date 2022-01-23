package com.github.leeonky.interpreter;

public interface ExpressionClauseFactory<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> {
    ExpressionClause<C, N> fetch(T tokenParser);

    default NodeFactory<C, N, E, O, T> input(N node) {
        return parser -> fetch(parser).makeExpression(node);
    }
}
