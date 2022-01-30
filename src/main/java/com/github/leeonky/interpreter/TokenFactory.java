package com.github.leeonky.interpreter;

import java.util.function.Function;

public interface TokenFactory {
    Token fetch(SourceCode sourceCode);

    default <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, S extends Scanner<C, N, E, O, S>> NodeFactory<C, N, E, O, S> map(
            Function<Token, N> mapper) {
        return scanner -> {
            Token token = fetch(scanner.getSourceCode());
            return mapper.apply(token).setPositionBegin(token.getPosition());
        };
    }
}
