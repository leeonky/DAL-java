package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface NodeFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {

    N fetch(TokenParser<E, N, C> parser);

    default NodeFactory<C, N, E> recursive(ExpressionMatcher<C, N, E> expressionMatcher) {
        return parser -> {
            N node = fetch(parser);
            Optional<N> optionalNode = expressionMatcher.fetch(parser, node);
            while (optionalNode.isPresent())
                optionalNode = expressionMatcher.fetch(parser, node = optionalNode.get());
            return node;
        };
    }

    default NodeFactory<C, N, E> map(Function<N, N> mapping) {
        return parser -> mapping.apply(fetch(parser));
    }

    default NodeFactory<C, N, E> withClause(ExpressionClauseFactory<C, N, E> expressionClauseFactory) {
        return parser -> {
            N node = fetch(parser);
            return expressionClauseFactory.fetch(parser).makeExpression(node);
        };
    }
}
