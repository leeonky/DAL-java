package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.Operators;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Operator;

public abstract class DALOperator extends Operator<DALRuntimeContext, DALNode, DALOperator, DALExpression> {
    private final boolean needInspect;
    private final Operators type;

    protected DALOperator(int precedence, String label, boolean needInspect, Operators type) {
        super(precedence, label);
        this.needInspect = needInspect;
        this.type = type;
    }

    public Data calculateData(DALExpression expression, DALRuntimeContext context) {
        return context.wrap(calculate(expression, context));
    }

    @Override
    public Object calculate(DALExpression expression, DALRuntimeContext context) {
        return calculateData(expression, context).instance();
    }

    public boolean isNeedInspect() {
        return needInspect;
    }

    public String inspect(String node1, String node2) {
        if (node1 == null || node1.isEmpty())
            return String.format("%s %s", label, node2);
        return String.format("%s %s %s", node1, label, node2);
    }

    public Operators overrideType() {
        return type;
    }
}
