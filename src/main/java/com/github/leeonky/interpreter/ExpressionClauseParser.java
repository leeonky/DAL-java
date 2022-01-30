package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.interpreter.ExpressionClauseParser.oneOf;
import static java.util.Optional.empty;

public interface ExpressionClauseParser<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {
    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> ExpressionClauseParser<C, N, E, O, P> oneOf(
            ExpressionClauseParser<C, N, E, O, P>... matchers) {
        return procedure -> Stream.of(matchers).map(p -> p.parse(procedure))
                .filter(Optional::isPresent).findFirst().orElse(empty());
    }

    Optional<ExpressionClause<C, N>> parse(P procedure);

    default Mandatory<C, N, E, O, P> or(Mandatory<C, N, E, O, P> expressionClauseMandatory) {
        return procedure -> parse(procedure).orElseGet(() -> expressionClauseMandatory.parse(procedure));
    }

    @SuppressWarnings("unchecked")
    default ExpressionClauseParser<C, N, E, O, P> concat(ExpressionClauseParser<C, N, E, O, P>... clauses) {
        return procedure -> {
            Optional<ExpressionClause<C, N>> optionalExpressionClause = parse(procedure);
            if (optionalExpressionClause.isPresent()) {
                Optional<ExpressionClause<C, N>> nextOptionalClause = oneOf(clauses).parse(procedure);
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
        return procedure -> parse(procedure).map(clause -> clause.makeExpression(input));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>,
            E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, S extends Procedure<C, N, E, O, S>> {
        ExpressionClause<C, N> parse(S procedure);

        default NodeParser.Mandatory<C, N, E, O, S> input(N node) {
            return procedure -> parse(procedure).makeExpression(node);
        }

        //    TODO token => to class like operaterMatcher::toClause
        @Deprecated
        static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>, O extends Operator<C, N, O>,
                S extends Procedure<C, N, E, O, S>> ExpressionClauseParser<C, N, E, O, S> after(
                String token, Mandatory<C, N, E, O, S> expressionClauseMandatory) {
            return procedure -> procedure.fetchClauseAfter(token, expressionClauseMandatory);
        }
    }
}
