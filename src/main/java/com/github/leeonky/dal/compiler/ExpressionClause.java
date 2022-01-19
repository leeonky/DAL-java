package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ListEllipsisNode;
import com.github.leeonky.dal.ast.Node;

import java.util.function.Function;

public interface ExpressionClause<N extends Node<N>> {
    N makeExpression(N input);

    default ExpressionClause<N> map(Function<N, N> mapper) {
        return node -> mapper.apply(makeExpression(node));
    }

    default boolean isListEllipsis() {
        return makeExpression(null) instanceof ListEllipsisNode;
    }
}
