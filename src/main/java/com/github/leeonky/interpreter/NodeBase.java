package com.github.leeonky.interpreter;

public abstract class NodeBase<N extends NodeBase<N, C>, C extends RuntimeContext<C>> implements Node<N, C> {
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
