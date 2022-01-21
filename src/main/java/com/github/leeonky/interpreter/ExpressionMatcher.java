package com.github.leeonky.interpreter;

import java.util.Optional;

public interface ExpressionMatcher<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {
    Optional<N> fetch(TokenParser<E, N, C> tokenParser, N previous);

    default NodeMatcher<C, N, E> defaultInputNode(N instance) {
        return parser -> fetch(parser, instance);
    }
}
