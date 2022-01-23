package com.github.leeonky.interpreter;

import java.util.Optional;

public interface ExpressionClauseMatcher<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> {
    Optional<ExpressionClause<C, N>> fetch(T tokenParser);

    default ExpressionClauseFactory<C, N, E, O, T> or(ExpressionClauseFactory<C, N, E, O, T> expressionClauseFactory) {
        return parser -> fetch(parser).orElseGet(() -> expressionClauseFactory.fetch(parser));
    }
}
