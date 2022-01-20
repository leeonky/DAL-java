package com.github.leeonky.interpreter;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.Operator;

public interface ExpressionConstructor<N extends Node<N>> {
    N newInstance(N node1, Operator<N> operator, N node2);
}
