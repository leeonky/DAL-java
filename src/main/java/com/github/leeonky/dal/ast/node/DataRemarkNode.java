package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class DataRemarkNode extends DALNode {
    private final String remark;

    public DataRemarkNode(String remark) {
        this.remark = remark;
    }

    @Override
    public String inspect() {
        return remark;
    }

    @Override
    public Object evaluate(RuntimeContextBuilder.DALRuntimeContext context) {
        return remark;
    }
}
