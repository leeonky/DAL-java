package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Operator;

import java.util.Objects;

//TODO to be generic
public class Expression extends DALNode {
    private final DALNode node1;
    private final Operator<DALNode, DALRuntimeContext> operator;
    private final DALNode node2;

    public Expression(DALNode node1, Operator<DALNode, DALRuntimeContext> operator, DALNode node2) {
        this.node1 = node1;
        this.node2 = node2;
        this.operator = operator;
        setPositionBegin(operator.getPosition());
    }

    public DALNode getLeftOperand() {
        return node1;
    }

    public DALNode getRightOperand() {
        return node2;
    }

    public Operator<DALNode, DALRuntimeContext> getOperator() {
        return operator;
    }

    @Override
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
    public Object evaluate(DALRuntimeContext context) {
        try {
            Object result = operator.calculate(node1, node2, context);
            if (operator.isNeedInspect() && (result instanceof Boolean) && !(boolean) result)
                System.err.println("Warning: Expression `" + inspect() + "` got false.");
            return result;
        } catch (IllegalArgumentException ex) {
            throw new RuntimeException(ex.getMessage(), operator.getPosition());
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
        return operator.inspect(node1 == null ? null : node1.inspect(), node2.inspect());
    }

    @Override
    public Object getRootName() {
        return node1.getRootName();
    }

    @Override
    public String inspectClause() {
        if (node1 instanceof SchemaExpression)
            return operator.inspect(node1.inspectClause(), node2.inspect());
        return operator.inspect("", node2.inspect());
    }

    @Override
    public int getOperandPosition() {
        return node2.getPositionBegin();
    }
}
