package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.interpreter.Expression;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class DALExpression extends DALNode implements Expression<DALRuntimeContext, DALNode, DALExpression, DALOperator> {
    private final DALNode node1;
    private final DALOperator operator;
    private final DALNode node2;

    public DALExpression(DALNode node1, DALOperator operator, DALNode node2) {
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
    public DALOperator getOperator() {
        return operator;
    }

    @Override
    public Data evaluateData(DALRuntimeContext context) {
        try {
            Data result = operator.calculateData(node1, node2, context);
            if (operator.isNeedInspect() && (result.getInstance() instanceof Boolean) && !(boolean) result.getInstance())
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
    public Object getRootSymbolName() {
        return node1 instanceof InputNode ? node2.getRootSymbolName() : node1.getRootSymbolName();
    }

    @Override
    public int getOperandPosition() {
        return node2.getPositionBegin();
    }

    @Override
    public List<Object> propertyChain() {
        return new ArrayList<Object>() {{
            addAll(node1.propertyChain());
            addAll(node2.propertyChain());
        }};
    }

//    @Override
//    public DALNode avoidListMapping() {
//        return node2.avoidListMapping();
//    }
}
