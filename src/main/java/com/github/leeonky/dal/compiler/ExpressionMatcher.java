package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;

public interface ExpressionMatcher<N extends Node<N>> {
    Optional<N> fetch(TokenParser<N> tokenParser, N previous);

    default NodeMatcher<N> defaultInputNode(N instance) {
        return parser -> fetch(parser, instance);
    }
}
