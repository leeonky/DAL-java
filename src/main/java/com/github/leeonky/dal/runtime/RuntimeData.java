package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class RuntimeData {
    protected final DALNode inputNode;
    protected final DALRuntimeContext runtimeContext;
    protected final Data data;

    public RuntimeData(Data data, DALNode inputNode, DALRuntimeContext runtimeContext) {
        this.inputNode = inputNode;
        this.runtimeContext = runtimeContext;
        this.data = data;
    }

    public DALNode inputNode() {
        return inputNode;
    }

    public DALRuntimeContext runtimeContext() {
        return runtimeContext;
    }

    public Data data() {
        return data;
    }
}
