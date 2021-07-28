package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;

public abstract class Node {
    private int positionBegin;

    public Object evaluate(RuntimeContext context) {
        throw new IllegalStateException();
    }

    public int getPositionBegin() {
        return positionBegin;
    }

    public void setPositionBegin(int positionBegin) {
        this.positionBegin = positionBegin;
    }

    public abstract String inspect();

    public boolean evaluable() {
        return true;
    }
}
