package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.Optional.empty;

public interface NodeMatcher<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, T extends Scanner<C, N, E, O, T>> {

    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, T extends Scanner<C, N, E, O, T>> NodeMatcher<C, N, E, O, T> oneOf(
            NodeMatcher<C, N, E, O, T> matcher, NodeMatcher<C, N, E, O, T>... matchers) {
        return parser -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(parser)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

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
