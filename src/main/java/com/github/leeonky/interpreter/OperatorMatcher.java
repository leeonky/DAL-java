package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.stream.Stream;

import static java.util.Optional.empty;

public interface OperatorMatcher<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> {
    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> OperatorMatcher<C, N, E, O, T> oneOf(
            OperatorMatcher<C, N, E, O, T> matcher, OperatorMatcher<C, N, E, O, T>... matchers) {
        return parser -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(parser)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    Optional<O> fetch(T tokenParser);

    default OperatorFactory<C, N, E, O, T> or(OperatorFactory<C, N, E, O, T> compiler) {
        return tokenParser -> fetch(tokenParser).orElseGet(() -> compiler.fetch(tokenParser));
    }

    default OperatorFactory<C, N, E, O, T> or(String message) {
        return tokenParser -> fetch(tokenParser).orElseThrow(() -> tokenParser.getSourceCode().syntaxError(message, 0));
    }

    default ExpressionClauseMatcher<C, N, E, O, T> toClause(NodeFactory<C, N, E, O, T> nodeFactory) {
        return parser -> parser.fetchExpressionClause(this, nodeFactory);
    }
}
