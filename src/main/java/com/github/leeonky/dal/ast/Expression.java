package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.ExpressionConstructor;
import com.github.leeonky.interpreter.Operator;

public abstract class Expression extends DALNode {
    public abstract DALNode getLeftOperand();

    public abstract DALNode getRightOperand();

    public abstract Operator<DALNode, RuntimeContextBuilder.DALRuntimeContext> getOperator();

    @Override
    public abstract Expression adjustOperatorOrder(ExpressionConstructor<DALNode, RuntimeContextBuilder.DALRuntimeContext> expressionConstructor);

    @Override
    public abstract Object evaluate(RuntimeContextBuilder.DALRuntimeContext context);

    @Override
    public abstract int getOperandPosition();
}
