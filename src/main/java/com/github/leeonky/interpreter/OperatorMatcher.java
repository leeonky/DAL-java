package com.github.leeonky.interpreter;

import java.util.Optional;

public interface OperatorMatcher<N extends Node<N, C>, C extends RuntimeContext<C>> {
    Optional<Operator<N, C>> fetch(TokenParser<N, C> tokenParser);

    default OperatorFactory<N, C> or(OperatorFactory<N, C> compiler) {
        return tokenParser -> fetch(tokenParser).orElseGet(() -> compiler.fetch(tokenParser));
    }

    default OperatorFactory<N, C> or(String message) {
        return tokenParser -> fetch(tokenParser).orElseThrow(() -> tokenParser.getSourceCode().syntaxError(message, 0));
    }
}
