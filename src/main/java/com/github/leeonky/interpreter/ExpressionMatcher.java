package com.github.leeonky.interpreter;

import java.util.Optional;

public interface ExpressionMatcher<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> {
    Optional<N> fetch(T tokenParser, N previous);

    default NodeMatcher<C, N, E, O, T> defaultInputNode(N instance) {
        return parser -> fetch(parser, instance);
    }
}
