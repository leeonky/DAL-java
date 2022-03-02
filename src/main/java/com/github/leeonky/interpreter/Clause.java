package com.github.leeonky.interpreter;

public interface Clause<C extends RuntimeContext<C>, N extends Node<C, N>> {
    N expression(N input);
}
