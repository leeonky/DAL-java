package com.github.leeonky.interpreter;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class Node<N extends Node<N, C>, C extends RuntimeContext<C>> {
    protected int positionBegin;

    public Object evaluate(RuntimeContextBuilder.DALRuntimeContext context) {
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
