package com.github.leeonky.dal.ast;

public class HeaderNode extends Node {
    private final Node property;
    private final Operator operator;

    //TODO default table operator
    public HeaderNode(Node property, Operator operator) {
        this.property = property;
        this.operator = operator;
    }

    @Override
    public String inspect() {
        return operator.inspect(property.inspect(), "").trim();
    }
}
