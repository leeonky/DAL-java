package com.github.leeonky.interpreter;

public interface Expression<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>> extends Node<C, N> {

    N getLeftOperand();

    N getRightOperand();

    O getOperator();

    @SuppressWarnings("unchecked")
    default N applyPrecedence(ExpressionFactory<C, N, E, O> factory) {
        if (getLeftOperand() instanceof Expression) {
            E leftExpression = (E) getLeftOperand();
            if (getOperator().isPrecedentThan(leftExpression.getOperator()))
                return (N) factory.create(leftExpression.getLeftOperand(), leftExpression.getOperator(),
                        factory.create(leftExpression.getRightOperand(), getOperator(), getRightOperand())
                                .applyPrecedence(factory));
        }
        return (N) this;
    }
}
