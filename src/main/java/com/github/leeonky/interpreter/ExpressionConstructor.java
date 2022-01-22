package com.github.leeonky.interpreter;

import com.github.leeonky.dal.ast.DALOperator;

public interface ExpressionConstructor<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {
    E newInstance(N node1, DALOperator operator, N node2);
}
