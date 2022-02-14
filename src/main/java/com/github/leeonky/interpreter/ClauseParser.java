package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static com.github.leeonky.interpreter.ClauseParser.oneOf;
import static java.util.Optional.empty;

public interface ClauseParser<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>>
        extends Parser<C, N, E, O, P, ClauseParser<C, N, E, O, P>,
        ClauseParser.Mandatory<C, N, E, O, P>, Clause<C, N>> {
    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> ClauseParser<C, N, E, O, P> oneOf(
            ClauseParser<C, N, E, O, P>... matchers) {
        return procedure -> Stream.of(matchers).map(p -> p.parse(procedure))
                .filter(Optional::isPresent).findFirst().orElse(empty());
    }

    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> ClauseParser<C, N, E, O, P> lazy(
            Supplier<ClauseParser<C, N, E, O, P>> parser) {
        return procedure -> parser.get().parse(procedure);
    }

    @Override
    default Mandatory<C, N, E, O, P> cast(Parser.Mandatory<C, N, E, O, P, ClauseParser<C, N, E, O, P>,
            Mandatory<C, N, E, O, P>, Clause<C, N>> mandatory) {
        return mandatory::parse;
    }

    @SuppressWarnings("unchecked")
    default ClauseParser<C, N, E, O, P> concat(ClauseParser<C, N, E, O, P>... clauses) {
        return procedure -> {
            Optional<Clause<C, N>> optionalExpressionClause = parse(procedure);
            if (optionalExpressionClause.isPresent()) {
                Optional<Clause<C, N>> nextOptionalClause = oneOf(clauses).parse(procedure);
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
            E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>>
            extends Parser.Mandatory<C, N, E, O, P, ClauseParser<C, N, E, O, P>, ClauseParser.Mandatory<C, N, E, O, P>,
            Clause<C, N>> {

        @Override
        default Mandatory<C, N, E, O, P> cast(Parser.Mandatory<C, N, E, O, P, ClauseParser<C, N, E, O, P>,
                Mandatory<C, N, E, O, P>, Clause<C, N>> mandatory) {
            return mandatory::parse;
        }

        default NodeParser.Mandatory<C, N, E, O, P> input(N node) {
            return procedure -> parse(procedure).makeExpression(node);
        }
    }
}
