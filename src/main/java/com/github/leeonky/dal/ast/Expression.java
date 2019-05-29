package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.RuntimeException;

import java.util.Objects;

public class Expression extends Node {
    private final Node node1, node2;

    private final Operator operator;

    public Expression(Node node1, Node node2, Operator operator) {
        this.node1 = node1;
        this.node2 = node2;
        this.operator = operator;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        Object v1 = node1.evaluate(context);
        Object v2 = node2.evaluate(context);
        try {
            return operator.calculate(v1, v2);
        } catch (IllegalArgumentException ex) {
            throw new RuntimeException(ex.getMessage(), operator.getPosition());
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof Expression
                && Objects.equals(node1, ((Expression) obj).node1)
                && Objects.equals(node2, ((Expression) obj).node2)
                && Objects.equals(operator, ((Expression) obj).operator)
                && Objects.equals(operator, ((Expression) obj).operator);
    }
}
