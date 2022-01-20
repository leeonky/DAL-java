package com.github.leeonky.interpreter;

import java.util.function.Function;

public interface TokenFactory {
    Token fetch(SourceCode sourceCode);

    default <N extends Node<N, C>, C extends RuntimeContext<C>> NodeFactory<N, C> map(Function<Token, N> mapper) {
        return parser -> {
            Token token = fetch(parser.getSourceCode());
            return mapper.apply(token).setPositionBegin(token.getPosition());
        };
    }
}
