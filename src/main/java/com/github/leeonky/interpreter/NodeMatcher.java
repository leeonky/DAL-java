package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface NodeMatcher<N extends Node<N, C>, C extends RuntimeContext<C>> {

    Optional<N> fetch(TokenParser<N, C> parser);

    default NodeMatcher<N, C> map(Function<N, N> mapping) {
        return parser -> fetch(parser).map(mapping);
    }

    default NodeFactory<N, C> or(NodeFactory<N, C> nodeFactory) {
        return parser -> fetch(parser).orElseGet(() -> nodeFactory.fetch(parser));
    }

    default NodeFactory<N, C> or(String message) {
        return parser -> fetch(parser).orElseThrow(() -> parser.getSourceCode().syntaxError(message, 0));
    }
}
