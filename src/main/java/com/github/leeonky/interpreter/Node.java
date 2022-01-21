package com.github.leeonky.interpreter;

public interface Node<N extends Node<N, C>, C extends RuntimeContext<C>> {

    default Object evaluate(C context) {
        throw new IllegalStateException();
    }

    int getPositionBegin();

    N setPositionBegin(int positionBegin);

    int getOperandPosition();

    //TODO move to expression
    @SuppressWarnings("unchecked")
    default N adjustOperatorOrder(ExpressionConstructor<N, C> expressionConstructor) {
        return (N) this;
    }
}
