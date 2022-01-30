package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.interpreter.ExpressionClauseParser.oneOf;
import static java.util.Optional.empty;

public interface ExpressionClauseParser<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Parser<C, N, E, O, P>> {
    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, P extends Parser<C, N, E, O, P>> ExpressionClauseParser<C, N, E, O, P> oneOf(
            ExpressionClauseParser<C, N, E, O, P>... matchers) {
        return parser -> Stream.of(matchers)
                .map(p -> p.parse(parser)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    Optional<ExpressionClause<C, N>> parse(P parser);

    default ExpressionClauseFactory<C, N, E, O, P> or(ExpressionClauseFactory<C, N, E, O, P> expressionClauseFactory) {
        return parser -> parse(parser).orElseGet(() -> expressionClauseFactory.parse(parser));
    }

    @SuppressWarnings("unchecked")
    default ExpressionClauseParser<C, N, E, O, P> concat(ExpressionClauseParser<C, N, E, O, P>... clauses) {
        return parser -> {
            Optional<ExpressionClause<C, N>> optionalExpressionClause = parse(parser);
            if (optionalExpressionClause.isPresent()) {
                Optional<ExpressionClause<C, N>> nextOptionalClause = oneOf(clauses).parse(parser);
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

    default NodeParser<C, N, E, O, P> defaultInputNode(N input) {
        return parser -> parse(parser).map(clause -> clause.makeExpression(input));
    }

    interface ExpressionClauseFactory<C extends RuntimeContext<C>, N extends Node<C, N>,
            E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, S extends Parser<C, N, E, O, S>> {
        ExpressionClause<C, N> parse(S parser);

        default NodeParser.Mandatory<C, N, E, O, S> input(N node) {
            return parser -> parse(parser).makeExpression(node);
        }

        //    TODO token => to class like operaterMatcher::toClause
        @Deprecated
        static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>, O extends Operator<C, N, O>,
                S extends Parser<C, N, E, O, S>> ExpressionClauseParser<C, N, E, O, S> after(
                String token, ExpressionClauseFactory<C, N, E, O, S> expressionClauseFactory) {
            return parser -> parser.fetchClauseAfter(token, expressionClauseFactory);
        }
    }
}
