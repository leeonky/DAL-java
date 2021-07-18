package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;

public abstract class Node {
    private int positionBegin;

    public abstract Object evaluate(RuntimeContext context);

    public int getPositionBegin() {
        return positionBegin;
    }

    public void setPositionBegin(int positionBegin) {
        this.positionBegin = positionBegin;
    }

    public abstract String inspect();
}
