package com.github.leeonky.interpreter;

import java.util.Optional;

public interface ExpressionClauseMatcher<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {
    Optional<ExpressionClause<C, N>> fetch(TokenParser<E, N, C> tokenParser);

    default ExpressionClauseFactory<C, N, E> or(ExpressionClauseFactory<C, N, E> expressionClauseFactory) {
        return parser -> fetch(parser).orElseGet(() -> expressionClauseFactory.fetch(parser));
    }
}
