package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

public interface ExpressionClauseFactory {
    ExpressionClause fetch(TokenParser tokenParser);

    default NodeFactory input(Node node) {
        return parser -> fetch(parser).makeExpression(node);
    }
}
