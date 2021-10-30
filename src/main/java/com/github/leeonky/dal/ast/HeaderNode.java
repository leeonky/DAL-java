package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.OperatorFactory;

public class HeaderNode extends Node {
    private final SequenceNode sequence;
    private final Node property;
    private final Operator operator;

    public HeaderNode(SequenceNode sequence, Node property, Operator operator) {
        this.sequence = sequence;
        this.property = property;
        this.operator = operator;
    }

    @Override
    public String inspect() {
        return sequence.inspect() + operator.inspect(property.inspect(), "").trim();
    }

    public Node getProperty() {
        return property;
    }

    public Operator getOperator() {
        return operator;
    }

    public OperatorFactory defaultHeaderOperator() {
        return tokenParser -> operator;
    }

    public SequenceNode getSequence() {
        return sequence;
    }
}
