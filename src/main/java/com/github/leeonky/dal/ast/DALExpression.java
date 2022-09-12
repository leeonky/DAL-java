package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.interpreter.Expression;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

public class DALExpression extends DALNode implements Expression<DALRuntimeContext, DALNode, DALExpression, DALOperator>,
        ExecutableNode {
    private final DALNode left;
    private final DALOperator operator;
    private final DALNode right;

    public DALExpression(DALNode left, DALOperator operator, DALNode right) {
        this.left = left;
        this.right = right;
        this.operator = operator;
        setPositionBegin(operator.getPosition());
    }

    @Override
    public DALNode getLeftOperand() {
        return left;
    }

    @Override
    public DALNode getRightOperand() {
        return right;
    }

    @Override
    public DALOperator getOperator() {
        return operator;
    }

    @Override
    public Data evaluateData(DALRuntimeContext context) {
        try {
            return logFalseResultAsNeeded(operator.calculateData(left, right, context));
        } catch (IllegalArgumentException ex) {
            throw new RuntimeException(ex.getMessage(), operator.getPosition());
        }
    }

    private Data logFalseResultAsNeeded(Data result) {
        if (operator.isNeedInspect() && (result.getInstance() instanceof Boolean) && !(boolean) result.getInstance())
            System.err.println("Warning: Expression `" + inspect() + "` got false.");
        return result;
    }

    @Override
    public String inspect() {
        return operator.inspect(left == null ? null : left.inspect(), right.inspect());
    }

    @Override
    public Object getRootSymbolName() {
        return left instanceof InputNode || isRootPropertyThis() ? right.getRootSymbolName() : left.getRootSymbolName();
    }

    private boolean isRootPropertyThis() {
        return left instanceof DALExpression && ((DALExpression) left).getRightOperand() instanceof PropertyThis;
    }

    @Override
    public int getOperandPosition() {
        return right.getPositionBegin();
    }

    @Override
    public List<Object> propertyChain() {
        return new ArrayList<Object>() {{
            addAll(left.propertyChain());
            addAll(right.propertyChain());
        }};
    }

    @Override
    public Stream<Object> collectFields(Data data) {
        if (((DALExpression) getLeftOperand()).getRightOperand() instanceof PropertyThis)
            if (getRightOperand() instanceof ObjectScopeNode)
                return getRightOperand().collectFields(data);
        return super.collectFields(data);
    }

    @Override
    public Data getValue(Data data, DALRuntimeContext context) {
        return context.newBlockScope(data, () -> evaluateData(context));
    }
}
