package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface NodeMatcher<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> {

    Optional<N> fetch(T parser);

    default NodeMatcher<C, N, E, O, T> map(Function<N, N> mapping) {
        return parser -> fetch(parser).map(mapping);
    }

    default NodeFactory<C, N, E, O, T> or(NodeFactory<C, N, E, O, T> nodeFactory) {
        return parser -> fetch(parser).orElseGet(() -> nodeFactory.fetch(parser));
    }

    default NodeFactory<C, N, E, O, T> or(String message) {
        return parser -> fetch(parser).orElseThrow(() -> parser.getSourceCode().syntaxError(message, 0));
    }
}
