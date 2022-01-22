package com.github.leeonky.interpreter;

import com.github.leeonky.dal.ast.DALOperator;

public abstract class Operator<C extends RuntimeContext<C>, N extends Node<C, N>> {
    protected final int precedence;
    protected final String label;
    private int position;

    public Operator(int precedence, String label) {
        this.precedence = precedence;
        this.label = label;
    }

    public boolean isPrecedentThan(DALOperator operator) {
        return precedence > operator.precedence;
    }

    public abstract Object calculate(N node1, N node2, C context);

    public int getPosition() {
        return position;
    }

    public DALOperator setPosition(int position) {
        this.position = position;
        return (DALOperator) this;
    }
}
