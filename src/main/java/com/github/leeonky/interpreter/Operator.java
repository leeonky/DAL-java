package com.github.leeonky.interpreter;

public abstract class Operator<C extends RuntimeContext<C>, N extends Node<C, N>, O extends Operator<C, N, O>> {
    protected final int precedence;
    protected final String label;
    private int position;

    public Operator(int precedence, String label) {
        this.precedence = precedence;
        this.label = label;
    }

    public boolean isPrecedentThan(O operator) {
        return precedence > operator.precedence;
    }

    public abstract Object calculate(N node1, N node2, C context);

    public int getPosition() {
        return position;
    }

    @SuppressWarnings("unchecked")
    public O setPosition(int position) {
        this.position = position;
        return (O) this;
    }
}
