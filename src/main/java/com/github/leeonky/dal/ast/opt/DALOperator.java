package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Operator;

public abstract class DALOperator extends Operator<DALRuntimeContext, DALNode, DALOperator> {
    private final boolean needInspect;

    protected DALOperator(int precedence, String label, boolean needInspect) {
        super(precedence, label);
        this.needInspect = needInspect;
    }

    public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
        return context.wrap(calculate(left, right, context));
    }

    @Override
    public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
        return calculateData(left, right, context).getInstance();
    }

    public boolean isNeedInspect() {
        return needInspect;
    }

    public String inspect(String node1, String node2) {
        if (node1 == null || node1.isEmpty())
            return String.format("%s %s", label, node2);
        return String.format("%s %s %s", node1, label, node2);
    }
}
