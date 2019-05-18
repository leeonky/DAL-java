package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.ConstValue;
import com.github.leeonky.dal.ast.Evaluatable;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputValue;

public class DALCompiler {
    public Evaluatable compile(String expressionContent) {
        return new Expression(new InputValue(), new ConstValue(Integer.valueOf(expressionContent.substring(1).trim())));
    }
}
