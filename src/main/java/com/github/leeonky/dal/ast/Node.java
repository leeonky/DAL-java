package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;

public abstract class Node {
    private int positionBegin, positionEnd;

    public abstract Object evaluate(RuntimeContext context);

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
