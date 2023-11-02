package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class RuntimeData {
    protected final DALNode inputNode;
    protected final DALNode OperandNode;
    protected final DALRuntimeContext runtimeContext;
    protected final Data data;
    private final DALOperator operator;

    public RuntimeData(Data data, DALNode inputNode, DALNode operandNode,
                       DALRuntimeContext runtimeContext, DALOperator operator) {
        this.inputNode = inputNode;
        OperandNode = operandNode;
        this.runtimeContext = runtimeContext;
        this.data = data;
        this.operator = operator;
    }

    public DALNode inputNode() {
        return inputNode;
    }

    public DALNode operandNode() {
        return OperandNode;
    }

    public DALRuntimeContext runtimeContext() {
        return runtimeContext;
    }

    public Data data() {
        return data;
    }

    public DALOperator operator() {
        return operator;
    }
}
