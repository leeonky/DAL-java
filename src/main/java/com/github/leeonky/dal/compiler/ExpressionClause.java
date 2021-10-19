package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

public interface ExpressionClause {
    Node makeExpression(Node input);
}
