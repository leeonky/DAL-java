package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ListEllipsisNode;
import com.github.leeonky.dal.ast.Node;

import java.util.function.Function;

public interface ExpressionClause {
    Node makeExpression(Node input);

    default ExpressionClause map(Function<Node, Node> mapper) {
        return node -> mapper.apply(makeExpression(node));
    }

    default boolean isListEllipsis() {
        return makeExpression(null) instanceof ListEllipsisNode;
    }
}
