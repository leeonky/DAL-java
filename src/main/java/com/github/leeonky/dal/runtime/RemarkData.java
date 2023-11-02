package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class RemarkData extends RuntimeData {
    public RemarkData(Data data, DALNode inputNode, DALNode remarkNode,
                      DALRuntimeContext runtimeContext, DALOperator operator) {
        super(data, inputNode, remarkNode, runtimeContext, operator);
    }

    public String remark() {
        return operandNode().inspect();
    }
}
