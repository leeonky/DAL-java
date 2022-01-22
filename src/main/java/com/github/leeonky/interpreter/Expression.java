package com.github.leeonky.interpreter;

import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public interface Expression<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> extends Node<C, N> {
    N getLeftOperand();

    N getRightOperand();

    DALOperator getOperator();

    @SuppressWarnings("unchecked")
    default N adjustOperatorOrder(ExpressionConstructor<C, N, E> expressionConstructor) {
        return (N) this;
    }

    Object evaluate(RuntimeContextBuilder.DALRuntimeContext context);

    @Override
    int getOperandPosition();
}
