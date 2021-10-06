package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;
import java.util.function.Function;

public interface NodeMatcher {

    Optional<Node> fetch(TokenParser parser);

    default NodeMatcher map(Function<Node, Node> mapping) {
        return parser -> fetch(parser).map(mapping);
    }

    default NodeFactory or(NodeFactory nodeFactory) {
        return parser -> fetch(parser).orElseGet(() -> nodeFactory.fetch(parser));
    }

    default NodeFactory or(String message) {
        return parser -> fetch(parser).orElseThrow(() -> new SyntaxException(message, parser.getPosition()));
    }
}
