package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class Equal extends DALOperator {
    public Equal() {
        super(Precedence.COMPARISON, "=", true);
    }

    @Override
    public Data calculateData(DALExpression expression, RuntimeContextBuilder.DALRuntimeContext context) {
        return expression.right().verify(expression.left(), this, context);
    }

    @Override
    public String inspect(String node1, String node2) {
        return String.format("%s%s %s", node1, label, node2);
    }
}
