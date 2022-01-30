package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.stream.Stream;

import static java.util.Optional.empty;

public interface OperatorParser<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Parser<C, N, E, O, P>> {
    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, S extends Parser<C, N, E, O, S>> OperatorParser<C, N, E, O, S> oneOf(
            OperatorParser<C, N, E, O, S>... parsers) {
        return parser -> Stream.of(parsers).map(operatorParser -> operatorParser.parse(parser)).
                filter(Optional::isPresent).findFirst().orElse(empty());
    }

    Optional<O> parse(P parser);

    default Mandatory<C, N, E, O, P> or(Mandatory<C, N, E, O, P> compiler) {
        return parser -> parse(parser).orElseGet(() -> compiler.parse(parser));
    }

    default Mandatory<C, N, E, O, P> or(String message) {
        return parser -> parse(parser).orElseThrow(() -> parser.getSourceCode().syntaxError(message, 0));
    }

    default ExpressionClauseParser<C, N, E, O, P> toClause(NodeParser.Mandatory<C, N, E, O, P> nodeFactory) {
        return parser -> parser.fetchExpressionClause(this, nodeFactory);
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, S extends Parser<C, N, E, O, S>> {
        O parse(S parser);
    }
}
