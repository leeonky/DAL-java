package com.github.leeonky.interpreter;

import java.util.Optional;

public interface OperatorMatcher<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> {
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
