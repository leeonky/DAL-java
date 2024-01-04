package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class RemarkData extends RuntimeData {
    private final String remark;

    public RemarkData(Data data, DALNode inputNode, DALNode remarkNode, DALRuntimeContext runtimeContext, String remark) {
        super(data, runtimeContext);
        this.remark = remark;
    }

    public String remark() {
        return remark;
    }
}
