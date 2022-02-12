package com.github.leeonky.interpreter;

import java.util.Optional;

public interface ParserPx<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, T> {

    Optional<T> parse(P procedure);

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, T> {
        T parse(P procedure);
    }
}
