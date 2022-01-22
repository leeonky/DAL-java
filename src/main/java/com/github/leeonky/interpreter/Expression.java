package com.github.leeonky.interpreter;

public interface Expression<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>> extends Node<C, N> {

    N getLeftOperand();

    N getRightOperand();

    O getOperator();

    @SuppressWarnings("unchecked")
    default N adjustOperatorOrder(ExpressionConstructor<C, N, E, O> expressionConstructor) {
        return (N) this;
    }
}
