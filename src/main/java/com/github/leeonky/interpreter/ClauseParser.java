package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Supplier;

public interface ClauseParser<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>>
        extends Parser<C, N, E, O, P, ClauseParser<C, N, E, O, P>,
        ClauseParser.Mandatory<C, N, E, O, P>, Clause<C, N>> {

    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> ClauseParser<C, N, E, O, P> lazy(
            Supplier<ClauseParser<C, N, E, O, P>> parser) {
        return procedure -> parser.get().parse(procedure);
    }

    @Override
    default ClauseParser<C, N, E, O, P> castParser(Parser<C, N, E, O, P, ClauseParser<C, N, E, O, P>,
            Mandatory<C, N, E, O, P>, Clause<C, N>> op) {
        return op::parse;
    }

    @Override
    default Mandatory<C, N, E, O, P> castMandatory(Parser.Mandatory<C, N, E, O, P, ClauseParser<C, N, E, O, P>,
            Mandatory<C, N, E, O, P>, Clause<C, N>> mandatory) {
        return mandatory::parse;
    }

    default ClauseParser<C, N, E, O, P> concat(ClauseParser<C, N, E, O, P> clause) {
        return procedure -> {
            Optional<Clause<C, N>> optionalExpressionClause = parse(procedure);
            if (optionalExpressionClause.isPresent()) {
                Optional<Clause<C, N>> nextOptionalClause = clause.parse(procedure);
                if (nextOptionalClause.isPresent()) {
                    return Optional.of(previous -> {
                        N input = optionalExpressionClause.get().expression(previous);
                        return nextOptionalClause.get().expression(input).setPositionBegin(input.getPositionBegin());
                    });
                }
            }
            return optionalExpressionClause;
        };
    }

    default NodeParser<C, N, E, O, P> defaultInputNode(N input) {
        return procedure -> parse(procedure).map(clause -> clause.expression(input));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>,
            E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>>
            extends Parser.Mandatory<C, N, E, O, P, ClauseParser<C, N, E, O, P>, ClauseParser.Mandatory<C, N, E, O, P>,
            Clause<C, N>> {

        @Override
        default ClauseParser<C, N, E, O, P> castParser(Parser<C, N, E, O, P, ClauseParser<C, N, E, O, P>,
                Mandatory<C, N, E, O, P>, Clause<C, N>> op) {
            return op::parse;
        }

        default NodeParser.Mandatory<C, N, E, O, P> input(N node) {
            return procedure -> parse(procedure).expression(node);
        }
    }
}
