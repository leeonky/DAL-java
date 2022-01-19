package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;

public interface ExpressionClauseMatcher<N extends Node<N>> {
    Optional<ExpressionClause<N>> fetch(TokenParser<N> tokenParser);

    default ExpressionClauseFactory<N> or(ExpressionClauseFactory<N> expressionClauseFactory) {
        return parser -> fetch(parser).orElseGet(() -> expressionClauseFactory.fetch(parser));
    }
}
