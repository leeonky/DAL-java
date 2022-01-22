package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface NodeFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>> {

    N fetch(TokenParser<C, N, E, O> parser);

    default NodeFactory<C, N, E, O> recursive(ExpressionMatcher<C, N, E, O> expressionMatcher) {
        return parser -> {
            N node = fetch(parser);
            Optional<N> optionalNode = expressionMatcher.fetch(parser, node);
            while (optionalNode.isPresent())
                optionalNode = expressionMatcher.fetch(parser, node = optionalNode.get());
            return node;
        };
    }

    default NodeFactory<C, N, E, O> map(Function<N, N> mapping) {
        return parser -> mapping.apply(fetch(parser));
    }

    default NodeFactory<C, N, E, O> withClause(ExpressionClauseFactory<C, N, E, O> expressionClauseFactory) {
        return parser -> {
            N node = fetch(parser);
            return expressionClauseFactory.fetch(parser).makeExpression(node);
        };
    }
}
