package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

public interface ExpressionClauseFactory<N extends Node<N>> {
    ExpressionClause<N> fetch(TokenParser<N> tokenParser);

    default NodeFactory<N> input(N node) {
        return parser -> fetch(parser).makeExpression(node);
    }
}
