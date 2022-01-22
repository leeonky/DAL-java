package com.github.leeonky.interpreter;

public interface ExpressionClauseFactory<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>> {
    ExpressionClause<C, N> fetch(TokenParser<C, N, E, O> tokenParser);

    default NodeFactory<C, N, E, O> input(N node) {
        return parser -> fetch(parser).makeExpression(node);
    }
}
