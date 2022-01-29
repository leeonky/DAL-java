package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface NodeFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> {

    N fetch(T parser);

    default NodeFactory<C, N, E, O, T> map(Function<N, N> mapping) {
        return parser -> mapping.apply(fetch(parser));
    }

    default NodeFactory<C, N, E, O, T> withClause(ExpressionClauseFactory<C, N, E, O, T> expressionClauseFactory) {
        return parser -> {
            N node = fetch(parser);
            return expressionClauseFactory.fetch(parser).makeExpression(node);
        };
    }

    default NodeFactory<C, N, E, O, T> recursive(ExpressionClauseMatcher<C, N, E, O, T> expressionClauseMatcher) {
        return parser -> {
            N node = fetch(parser);
            Optional<ExpressionClause<C, N>> optionalNode = expressionClauseMatcher.fetch(parser);
            while (optionalNode.isPresent()) {
                node = optionalNode.get().makeExpression(node);
                optionalNode = expressionClauseMatcher.fetch(parser);
            }
            return node;
        };
    }
}
