package com.github.leeonky.interpreter;

import java.util.function.Function;

public interface TokenFactory {
    Token fetch(SourceCode sourceCode);

    default <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>, O extends Operator<C, N, O>> NodeFactory<C, N, E, O> map(Function<Token, N> mapper) {
        return parser -> {
            Token token = fetch(parser.getSourceCode());
            return mapper.apply(token).setPositionBegin(token.getPosition());
        };
    }
}
