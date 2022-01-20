package com.github.leeonky.interpreter;

public interface ExpressionClauseFactory<N extends Node<N, C>, C extends RuntimeContext<C>> {
    ExpressionClause<N, C> fetch(TokenParser<N, C> tokenParser);

    default NodeFactory<N, C> input(N node) {
        return parser -> fetch(parser).makeExpression(node);
    }
}
