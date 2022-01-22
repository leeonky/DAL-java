package com.github.leeonky.interpreter;

import com.github.leeonky.dal.ast.DALOperator;

public interface OperatorFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {
    DALOperator fetch(TokenParser<E, N, C> tokenParser);
}
