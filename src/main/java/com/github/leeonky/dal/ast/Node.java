package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

public abstract class Node {
    private int positionBegin, positionEnd;

    public abstract Object evaluate(CompilingContext context);

    public int getPositionEnd() {
        return positionEnd;
    }

    public void setPositionEnd(int positionEnd) {
        this.positionEnd = positionEnd;
    }

    public int getPositionBegin() {
        return positionBegin;
    }

    public void setPositionBegin(int positionBegin) {
        this.positionBegin = positionBegin;
    }
}
