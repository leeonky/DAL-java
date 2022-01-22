package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface NodeMatcher<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>> {

    Optional<N> fetch(TokenParser<C, N, E, O> parser);

    default NodeMatcher<C, N, E, O> map(Function<N, N> mapping) {
        return parser -> fetch(parser).map(mapping);
    }

    default NodeFactory<C, N, E, O> or(NodeFactory<C, N, E, O> nodeFactory) {
        return parser -> fetch(parser).orElseGet(() -> nodeFactory.fetch(parser));
    }

    default NodeFactory<C, N, E, O> or(String message) {
        return parser -> fetch(parser).orElseThrow(() -> parser.getSourceCode().syntaxError(message, 0));
    }
}
