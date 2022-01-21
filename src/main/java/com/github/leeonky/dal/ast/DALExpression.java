package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Expression;
import com.github.leeonky.interpreter.ExpressionConstructor;
import com.github.leeonky.interpreter.Operator;

import java.util.Objects;

public class DALExpression extends DALNode implements Expression<DALRuntimeContext, DALNode, DALExpression> {
    private final DALNode node1;
    private final Operator<DALRuntimeContext, DALNode> operator;
    private final DALNode node2;

    public DALExpression(DALNode node1, Operator<DALRuntimeContext, DALNode> operator, DALNode node2) {
        this.node1 = node1;
        this.node2 = node2;
        this.operator = operator;
        setPositionBegin(operator.getPosition());
    }

    @Override
    public DALNode getLeftOperand() {
        return node1;
    }

    @Override
    public DALNode getRightOperand() {
        return node2;
    }

    @Override
    public Operator<DALRuntimeContext, DALNode> getOperator() {
        return operator;
    }

    @Override
    public DALExpression adjustOperatorOrder(ExpressionConstructor<DALRuntimeContext, DALNode, DALExpression> expressionConstructor) {
        if (getLeftOperand() instanceof DALExpression) {
            DALExpression leftExpression = (DALExpression) getLeftOperand();
            if (getOperator().isPrecedentThan(leftExpression.getOperator()))
                return new DALExpression(leftExpression.getLeftOperand(), leftExpression.getOperator(),
                        new DALExpression(leftExpression.getRightOperand(), getOperator(), getRightOperand()).adjustOperatorOrder(expressionConstructor));
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
        return obj instanceof DALExpression
                && Objects.equals(node1, ((DALExpression) obj).node1)
                && Objects.equals(node2, ((DALExpression) obj).node2)
                && Objects.equals(operator, ((DALExpression) obj).operator);
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
