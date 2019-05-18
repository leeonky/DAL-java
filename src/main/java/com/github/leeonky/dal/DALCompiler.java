package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.Expression;

public class DALCompiler {
    public Evaluatable compile(String expressionContent) {
        Expression expression = new Expression();
        expression.setRightValue(Integer.valueOf(expressionContent.substring(1).trim()));
        return expression;
    }
}
