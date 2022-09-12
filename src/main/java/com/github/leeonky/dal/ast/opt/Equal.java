package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class Equal extends DALOperator {
    public Equal() {
        super(Precedence.COMPARISON, "=", true);
    }

    @Override
    public Object calculate(DALNode left, DALNode right, RuntimeContextBuilder.DALRuntimeContext context) {
        return left.verifyBy(right, this, context);
    }

    @Override
    public String inspect(String node1, String node2) {
        return String.format("%s%s %s", node1, label, node2);
    }
}
