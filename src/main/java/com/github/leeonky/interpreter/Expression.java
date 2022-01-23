package com.github.leeonky.interpreter;

public interface Expression<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>> extends Node<C, N> {

    N getLeftOperand();

    N getRightOperand();

    O getOperator();

    @SuppressWarnings("unchecked")
    default N adjustOperatorOrder(ExpressionConstructor<C, N, E, O> constructor) {
        if (getLeftOperand() instanceof Expression) {
            E leftExpression = (E) getLeftOperand();
            if (getOperator().isPrecedentThan(leftExpression.getOperator()))
                return (N) constructor.newInstance(leftExpression.getLeftOperand(), leftExpression.getOperator(),
                        constructor.newInstance(leftExpression.getRightOperand(), getOperator(), getRightOperand())
                                .adjustOperatorOrder(constructor));
        }
        return (N) this;
    }
}
