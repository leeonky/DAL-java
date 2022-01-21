package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface TokenMatcher<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {
    Optional<Token> fetch(SourceCode sourceCode);

    default TokenFactory or(String message) {
        return sourceCode -> fetch(sourceCode).orElseThrow(() -> sourceCode.syntaxError(message, 0));
    }

    default NodeMatcher<C, N, E> map(Function<Token, N> mapper) {
        return parser -> fetch(parser.getSourceCode()).map(token ->
                mapper.apply(token).setPositionBegin(token.getPosition()));
    }
}
