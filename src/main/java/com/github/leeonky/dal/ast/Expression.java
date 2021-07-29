package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;

import java.util.Objects;

public class Expression extends Node {
    private final Node node1;
    private final Operator operator;
    private final Node node2;

    public Expression(Node node1, Operator operator, Node node2) {
        this.node1 = node1;
        this.node2 = node2;
        this.operator = operator;
    }

    public Expression adjustOperatorOrder() {
        if (node1 instanceof Expression) {
            Expression expression1 = (Expression) node1;
            if (operator.isPrecedentThan(expression1.operator))
                return new Expression(expression1.node1, expression1.operator,
                        new Expression(expression1.node2, operator, node2).adjustOperatorOrder());
        }
        return this;
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        try {
            Object result = operator.calculate(node1, node2, context);
            if (operator.isNeedInspect() && (result instanceof Boolean) && !(boolean) result)
                System.err.println("Warning: Expression `" + inspect() + "` got false.");
            return result;
        } catch (IllegalArgumentException ex) {
            throw new RuntimeException(ex.getMessage(), operator.getPosition());
        } catch (ArrayIndexOutOfBoundsException ex) {
            throw new RuntimeException("Array index out of range", operator.getPosition());
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof Expression
                && Objects.equals(node1, ((Expression) obj).node1)
                && Objects.equals(node2, ((Expression) obj).node2)
                && Objects.equals(operator, ((Expression) obj).operator);
    }

    @Override
    public String inspect() {
        return operator.inspect(node1, node2);
    }
}
