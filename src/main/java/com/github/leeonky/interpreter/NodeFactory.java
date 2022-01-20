package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface NodeFactory<N extends Node<N, C>, C extends RuntimeContext<C>> {

    N fetch(TokenParser<N, C> parser);

    default NodeFactory<N, C> recursive(ExpressionMatcher<N, C> expressionMatcher) {
        return parser -> {
            N node = fetch(parser);
            Optional<N> optionalNode = expressionMatcher.fetch(parser, node);
            while (optionalNode.isPresent())
                optionalNode = expressionMatcher.fetch(parser, node = optionalNode.get());
            return node;
        };
    }

    default NodeFactory<N, C> map(Function<N, N> mapping) {
        return parser -> mapping.apply(fetch(parser));
    }

    default NodeFactory<N, C> withClause(ExpressionClauseFactory<N, C> expressionClauseFactory) {
        return parser -> {
            N node = fetch(parser);
            return expressionClauseFactory.fetch(parser).makeExpression(node);
        };
    }
}
