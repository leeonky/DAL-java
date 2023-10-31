package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;

public class RemarkData {
    private final Data data;
    private final DALNode inputNode;
    private final String remark;
    private final RuntimeContextBuilder.DALRuntimeContext runtimeContext;

    public RemarkData(Data data, DALNode inputNode, String remark,
                      RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        this.data = data;
        this.inputNode = inputNode;
        this.remark = remark;
        this.runtimeContext = runtimeContext;
    }

    public DALNode inputNode() {
        return inputNode;
    }

    public String remark() {
        return remark;
    }

    public RuntimeContextBuilder.DALRuntimeContext runtimeContext() {
        return runtimeContext;
    }

    public Data data() {
        return data;
    }
}
