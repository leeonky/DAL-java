package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class Equal extends DALOperator {
    public Equal() {
        super(Precedence.COMPARISON, "=", true);
    }

    @Override
    public Data calculateData(DALNode left, DALNode right, RuntimeContextBuilder.DALRuntimeContext context) {
        return right.verify(left, this, context);
    }

    @Override
    public String inspect(String node1, String node2) {
        return String.format("%s%s %s", node1, label, node2);
    }
}
