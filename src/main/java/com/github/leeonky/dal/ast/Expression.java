package com.github.leeonky.dal.ast;

import com.github.leeonky.ArrayType;
import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.RuntimeException;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;

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
    public Object evaluate(CompilingContext context) {
        Object v1 = node1.evaluate(context);
        Object v2 = node2.evaluate(context);
        try {
            if (operator instanceof Operator.Index) {
                Optional<? extends ArrayType> optionalArrayType = context.getArrayTypes().entrySet().stream()
                        .filter(e -> e.getKey().isInstance(v1))
                        .map(Map.Entry::getValue)
                        .findFirst();
                if (optionalArrayType.isPresent())
                    return optionalArrayType.get().get(v1, (int) v2);
            }
            return operator.calculate(v1, v2);
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
                && Objects.equals(operator, ((Expression) obj).operator)
                && Objects.equals(operator, ((Expression) obj).operator);
    }
}
