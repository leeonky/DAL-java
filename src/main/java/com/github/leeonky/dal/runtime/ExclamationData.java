package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;

public class ExclamationData extends RuntimeData {
    private final String label;

    public ExclamationData(Data data, DALNode inputNode, DALNode operandNode, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        super(data, runtimeContext);
        label = operandNode.inspect();
    }

    public String label() {
        return label;
    }
}
