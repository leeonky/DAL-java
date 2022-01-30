package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.Optional.empty;

public interface NodeMatcher<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, S extends Scanner<C, N, E, O, S>> {

    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, S extends Scanner<C, N, E, O, S>> NodeMatcher<C, N, E, O, S> oneOf(
            NodeMatcher<C, N, E, O, S> matcher, NodeMatcher<C, N, E, O, S>... matchers) {
        return scanner -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(scanner)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    Optional<N> fetch(S scanner);

    default NodeMatcher<C, N, E, O, S> map(Function<N, N> mapping) {
        return scanner -> fetch(scanner).map(mapping);
    }

    default NodeFactory<C, N, E, O, S> or(NodeFactory<C, N, E, O, S> nodeFactory) {
        return scanner -> fetch(scanner).orElseGet(() -> nodeFactory.fetch(scanner));
    }

    default NodeFactory<C, N, E, O, S> or(String message) {
        return scanner -> fetch(scanner).orElseThrow(() -> scanner.getSourceCode().syntaxError(message, 0));
    }
}
