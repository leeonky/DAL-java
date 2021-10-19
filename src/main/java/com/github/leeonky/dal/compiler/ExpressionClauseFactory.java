package com.github.leeonky.dal.compiler;

public interface ExpressionClauseFactory {
    ExpressionClause fetch(TokenParser tokenParser);
}
