package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.Optional.empty;

public interface NodeParser<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, S extends Parser<C, N, E, O, S>> {

    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, S extends Parser<C, N, E, O, S>> NodeParser<C, N, E, O, S> oneOf(
            NodeParser<C, N, E, O, S> matcher, NodeParser<C, N, E, O, S>... matchers) {
        return parser -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.parse(parser)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    Optional<N> parse(S parser);

    default NodeParser<C, N, E, O, S> map(Function<N, N> mapping) {
        return parser -> parse(parser).map(mapping);
    }

    default Mandatory<C, N, E, O, S> or(Mandatory<C, N, E, O, S> nodeFactory) {
        return parser -> parse(parser).orElseGet(() -> nodeFactory.parse(parser));
    }

    default Mandatory<C, N, E, O, S> or(String message) {
        return parser -> parse(parser).orElseThrow(() -> parser.getSourceCode().syntaxError(message, 0));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, S extends Parser<C, N, E, O, S>> {

        N parse(S parser);

        default Mandatory<C, N, E, O, S> map(Function<N, N> mapping) {
            return parser -> mapping.apply(parse(parser));
        }

        default Mandatory<C, N, E, O, S> withClause(ExpressionClauseParser.ExpressionClauseFactory<C, N, E, O, S> expressionClauseFactory) {
            return parser -> {
                N node = parse(parser);
                return expressionClauseFactory.parse(parser).makeExpression(node);
            };
        }

        default Mandatory<C, N, E, O, S> recursive(ExpressionClauseParser<C, N, E, O, S> expressionClauseParser) {
            return parser -> {
                N node = parse(parser);
                Optional<ExpressionClause<C, N>> optionalNode = expressionClauseParser.parse(parser);
                while (optionalNode.isPresent()) {
                    node = optionalNode.get().makeExpression(node);
                    optionalNode = expressionClauseParser.parse(parser);
                }
                return node;
            };
        }
    }
}
