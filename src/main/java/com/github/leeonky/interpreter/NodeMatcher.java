package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface NodeMatcher<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {

    Optional<N> fetch(TokenParser<E, N, C> parser);

    default NodeMatcher<C, N, E> map(Function<N, N> mapping) {
        return parser -> fetch(parser).map(mapping);
    }

    default NodeFactory<C, N, E> or(NodeFactory<C, N, E> nodeFactory) {
        return parser -> fetch(parser).orElseGet(() -> nodeFactory.fetch(parser));
    }

    default NodeFactory<C, N, E> or(String message) {
        return parser -> fetch(parser).orElseThrow(() -> parser.getSourceCode().syntaxError(message, 0));
    }
}
