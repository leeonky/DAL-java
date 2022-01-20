package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class Node<N extends Node<N>> {
    protected int positionBegin;

    public Object evaluate(RuntimeContextBuilder.RuntimeContext context) {
        throw new IllegalStateException();
    }

    public int getPositionBegin() {
        return positionBegin;
    }

    public N setPositionBegin(int positionBegin) {
        this.positionBegin = positionBegin;
        return (N) this;
    }

    public int getOperandPosition() {
        return positionBegin;
    }

    //TODO move to expression
    public N adjustOperatorOrder() {
        return (N) this;
    }
}
