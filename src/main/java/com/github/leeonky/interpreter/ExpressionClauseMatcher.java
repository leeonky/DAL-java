package com.github.leeonky.interpreter;

import java.util.Optional;

public interface ExpressionClauseMatcher<N extends Node<N, C>, C extends RuntimeContext<C>> {
    Optional<ExpressionClause<N, C>> fetch(TokenParser<N, C> tokenParser);

    default ExpressionClauseFactory<N, C> or(ExpressionClauseFactory<N, C> expressionClauseFactory) {
        return parser -> fetch(parser).orElseGet(() -> expressionClauseFactory.fetch(parser));
    }
}
