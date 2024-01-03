package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class RuntimeData {
    protected final DALNode inputNode;
    protected final DALNode operandNode;
    protected final DALRuntimeContext runtimeContext;
    protected final Data data;

    public RuntimeData(Data data, DALNode inputNode, DALNode operandNode, DALRuntimeContext runtimeContext) {
        this.inputNode = inputNode;
        this.operandNode = operandNode;
        this.runtimeContext = runtimeContext;
        this.data = data;
    }

    public DALNode inputNode() {
        return inputNode;
    }

    public DALNode operandNode() {
        return operandNode;
    }

    public DALRuntimeContext runtimeContext() {
        return runtimeContext;
    }

    public Data data() {
        return data;
    }
}
