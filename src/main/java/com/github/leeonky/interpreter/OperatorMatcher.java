package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.stream.Stream;

import static java.util.Optional.empty;

public interface OperatorMatcher<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, S extends Scanner<C, N, E, O, S>> {
    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, T extends Scanner<C, N, E, O, T>> OperatorMatcher<C, N, E, O, T> oneOf(
            OperatorMatcher<C, N, E, O, T>... matchers) {
        return scanner -> Stream.of(matchers)
                .map(p -> p.fetch(scanner)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    Optional<O> fetch(S scanner);

    default OperatorFactory<C, N, E, O, S> or(OperatorFactory<C, N, E, O, S> compiler) {
        return scanner -> fetch(scanner).orElseGet(() -> compiler.fetch(scanner));
    }

    default OperatorFactory<C, N, E, O, S> or(String message) {
        return scanner -> fetch(scanner).orElseThrow(() -> scanner.getSourceCode().syntaxError(message, 0));
    }

    default ExpressionClauseMatcher<C, N, E, O, S> toClause(NodeFactory<C, N, E, O, S> nodeFactory) {
        return scanner -> scanner.fetchExpressionClause(this, nodeFactory);
    }
}
