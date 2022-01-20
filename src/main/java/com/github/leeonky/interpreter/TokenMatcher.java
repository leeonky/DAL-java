package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface TokenMatcher {
    Optional<Token> fetch(SourceCode sourceCode);

    default TokenFactory or(String message) {
        return sourceCode -> fetch(sourceCode).orElseThrow(() -> sourceCode.syntaxError(message, 0));
    }

    default <N extends Node<N, C>, C extends RuntimeContext<C>> NodeMatcher<N, C> map(Function<Token, N> mapper) {
        return parser -> fetch(parser.getSourceCode()).map(token ->
                mapper.apply(token).setPositionBegin(token.getPosition()));
    }
}
