package com.github.leeonky.interpreter;

import java.util.Optional;

public interface OperatorMatcher<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>, O extends Operator<C, N, O>> {
    Optional<O> fetch(TokenParser<C, N, E, O> tokenParser);

    default OperatorFactory<C, N, E, O> or(OperatorFactory<C, N, E, O> compiler) {
        return tokenParser -> fetch(tokenParser).orElseGet(() -> compiler.fetch(tokenParser));
    }

    default OperatorFactory<C, N, E, O> or(String message) {
        return tokenParser -> fetch(tokenParser).orElseThrow(() -> tokenParser.getSourceCode().syntaxError(message, 0));
    }
}
