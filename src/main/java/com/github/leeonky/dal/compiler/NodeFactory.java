package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;
import java.util.function.Function;

public interface NodeFactory<N extends Node<N>> {

    N fetch(TokenParser<N> parser);

    default NodeFactory<N> recursive(ExpressionMatcher<N> expressionMatcher) {
        return parser -> {
            N node = fetch(parser);
            Optional<N> optionalNode = expressionMatcher.fetch(parser, node);
            while (optionalNode.isPresent())
                optionalNode = expressionMatcher.fetch(parser, node = optionalNode.get());
            return node;
        };
    }

    default NodeFactory<N> map(Function<N, N> mapping) {
        return parser -> mapping.apply(fetch(parser));
    }

    default NodeFactory<N> withClause(ExpressionClauseFactory<N> expressionClauseFactory) {
        return parser -> {
            N node = fetch(parser);
            return expressionClauseFactory.fetch(parser).makeExpression(node);
        };
    }
}
