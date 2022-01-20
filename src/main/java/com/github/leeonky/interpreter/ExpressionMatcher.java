package com.github.leeonky.interpreter;

import java.util.Optional;

public interface ExpressionMatcher<N extends Node<N, C>, C extends RuntimeContext<C>> {
    Optional<N> fetch(TokenParser<N, C> tokenParser, N previous);

    default NodeMatcher<N, C> defaultInputNode(N instance) {
        return parser -> fetch(parser, instance);
    }
}
