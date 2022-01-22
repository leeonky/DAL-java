package com.github.leeonky.interpreter;

import java.util.Optional;

public interface ExpressionClauseMatcher<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>> {
    Optional<ExpressionClause<C, N>> fetch(TokenParser<C, N, E, O> tokenParser);

    default ExpressionClauseFactory<C, N, E, O> or(ExpressionClauseFactory<C, N, E, O> expressionClauseFactory) {
        return parser -> fetch(parser).orElseGet(() -> expressionClauseFactory.fetch(parser));
    }
}
