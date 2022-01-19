package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.Operator;

public interface OperatorFactory<N extends Node<N>> {
    Operator fetch(TokenParser<N> tokenParser);
}
