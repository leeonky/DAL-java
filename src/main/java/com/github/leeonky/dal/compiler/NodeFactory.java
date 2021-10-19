package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;
import java.util.function.Function;

public interface NodeFactory {

    Node fetch(TokenParser parser);

    default NodeFactory recursive(ExpressionMatcher expressionMatcher) {
        return parser -> {
            Node node = fetch(parser);
            Optional<Node> optionalNode = expressionMatcher.fetch(parser, node);
            while (optionalNode.isPresent())
                optionalNode = expressionMatcher.fetch(parser, node = optionalNode.get());
            return node;
        };
    }

    default NodeFactory map(Function<Node, Node> mapping) {
        return parser -> mapping.apply(fetch(parser));
    }

    default NodeFactory withClause(ExpressionClauseFactory expressionClauseFactory) {
        return parser -> {
            Node node = fetch(parser);
            return expressionClauseFactory.fetch(parser).makeExpression(node);
        };
    }
}
