package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;
import java.util.function.Function;

public interface NodeMatcher<N extends Node<N>> {

    Optional<N> fetch(TokenParser<N> parser);

    default NodeMatcher<N> map(Function<N, N> mapping) {
        return parser -> fetch(parser).map(mapping);
    }

    default NodeFactory<N> or(NodeFactory<N> nodeFactory) {
        return parser -> fetch(parser).orElseGet(() -> nodeFactory.fetch(parser));
    }

    default NodeFactory<N> or(String message) {
        return parser -> fetch(parser).orElseThrow(() -> parser.getSourceCode().syntaxError(message, 0));
    }
}
