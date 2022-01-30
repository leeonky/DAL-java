package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.interpreter.ExpressionClauseMatcher.oneOf;
import static java.util.Optional.empty;

public interface ExpressionClauseMatcher<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, S extends Scanner<C, N, E, O, S>> {
    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, S extends Scanner<C, N, E, O, S>> ExpressionClauseMatcher<C, N, E, O, S> oneOf(
            ExpressionClauseMatcher<C, N, E, O, S>... matchers) {
        return scanner -> Stream.of(matchers)
                .map(p -> p.fetch(scanner)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    Optional<ExpressionClause<C, N>> fetch(S scanner);

    default ExpressionClauseFactory<C, N, E, O, S> or(ExpressionClauseFactory<C, N, E, O, S> expressionClauseFactory) {
        return scanner -> fetch(scanner).orElseGet(() -> expressionClauseFactory.fetch(scanner));
    }

    @SuppressWarnings("unchecked")
    default ExpressionClauseMatcher<C, N, E, O, S> concat(ExpressionClauseMatcher<C, N, E, O, S>... clauses) {
        return scanner -> {
            Optional<ExpressionClause<C, N>> optionalExpressionClause = fetch(scanner);
            if (optionalExpressionClause.isPresent()) {
                Optional<ExpressionClause<C, N>> nextOptionalClause = oneOf(clauses).fetch(scanner);
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

    default NodeMatcher<C, N, E, O, S> defaultInputNode(N input) {
        return scanner -> fetch(scanner).map(clause -> clause.makeExpression(input));
    }
}
