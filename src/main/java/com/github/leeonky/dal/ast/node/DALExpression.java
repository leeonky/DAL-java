package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.IllegalOperationException;
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

    private DALExpression(DALNode left, DALOperator operator, DALNode right) {
        this.left = left;
        this.right = right;
        this.operator = operator;
        setPositionBegin(operator.getPosition());
    }

    public static DALNode expression(DALNode left, DALOperator operator, DALNode right) {
        if (left instanceof GroupExpression)
            return ((GroupExpression) left).append(operator, right);
        if (right instanceof GroupExpression)
            return ((GroupExpression) right).insert(left, operator);
        return new DALExpression(left, operator, right).applyPrecedence(DALExpression::new);
    }

    @Override
    public DALNode left() {
        return left;
    }

    @Override
    public DALNode right() {
        return right;
    }

    @Override
    public DALOperator operator() {
        return operator;
    }

    @Override
    public Data evaluateData(DALRuntimeContext context) {
        try {
            return operator.calculateData(this, context);
        } catch (IllegalOperationException ex) {
            throw new RuntimeException(ex.getMessage(), operator.getPosition());
        }
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
        return left instanceof DALExpression && ((DALExpression) left).right() instanceof PropertyThis;
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
        if (((DALExpression) left()).right() instanceof PropertyThis)
            if (right() instanceof ObjectScopeNode)
                return right().collectFields(data);
        return super.collectFields(data);
    }

    @Override
    public Data getValue(Data data, DALRuntimeContext context) {
        return context.pushAndExecute(data, () -> evaluateData(context));
    }

    @Override
    public int getPositionBegin() {
        if (left == null || left instanceof InputNode) return super.getPositionBegin();
        return Math.min(super.getPositionBegin(), left.getPositionBegin());
    }
}
