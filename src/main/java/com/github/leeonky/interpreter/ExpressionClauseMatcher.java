package com.github.leeonky.interpreter;

import java.util.Optional;

import static com.github.leeonky.dal.compiler.Compiler.oneOf;

public interface ExpressionClauseMatcher<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> {
    Optional<ExpressionClause<C, N>> fetch(T tokenParser);

    default ExpressionClauseFactory<C, N, E, O, T> or(ExpressionClauseFactory<C, N, E, O, T> expressionClauseFactory) {
        return parser -> fetch(parser).orElseGet(() -> expressionClauseFactory.fetch(parser));
    }

    @Deprecated
    default ExpressionMatcher<C, N, E, O, T> toExpressionMatcher() {
        return (parser, previous) -> fetch(parser).map(clause -> clause.makeExpression(previous));
    }

    default ExpressionClauseMatcher<C, N, E, O, T> concat(ExpressionClauseMatcher<C, N, E, O, T>... clauses) {
        return parser -> {
            Optional<ExpressionClause<C, N>> optionalExpressionClause = fetch(parser);
            if (optionalExpressionClause.isPresent()) {
                Optional<ExpressionClause<C, N>> nextOptionalClause = oneOf(clauses).fetch(parser);
                if (nextOptionalClause.isPresent()) {
                    return Optional.of(previous -> {
                        N input = optionalExpressionClause.get().makeExpression(previous);
                        return nextOptionalClause.get().makeExpression(input).setPositionBegin(input.getPositionBegin());
                    });
                }
            }
            return optionalExpressionClause;
        };
    }
}
