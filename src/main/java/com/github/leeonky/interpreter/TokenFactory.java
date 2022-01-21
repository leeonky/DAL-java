package com.github.leeonky.interpreter;

import java.util.function.Function;

public interface TokenFactory {
    Token fetch(SourceCode sourceCode);

    default <E extends Expression<C, N, E>, N extends Node<C, N>, C extends RuntimeContext<C>> NodeFactory<C, N, E> map(Function<Token, N> mapper) {
        return parser -> {
            Token token = fetch(parser.getSourceCode());
            return mapper.apply(token).setPositionBegin(token.getPosition());
        };
    }
}
