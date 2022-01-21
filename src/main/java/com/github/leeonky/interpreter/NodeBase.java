package com.github.leeonky.interpreter;

public abstract class NodeBase<C extends RuntimeContext<C>, N extends NodeBase<C, N>> implements Node<C, N> {
    protected int positionBegin;

    @Override
    public int getPositionBegin() {
        return positionBegin;
    }

    @Override
    @SuppressWarnings("unchecked")
    public N setPositionBegin(int positionBegin) {
        this.positionBegin = positionBegin;
        return (N) this;
    }

    @Override
    public int getOperandPosition() {
        return positionBegin;
    }
}
