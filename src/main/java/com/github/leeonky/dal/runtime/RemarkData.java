package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.node.DataRemarkNode;

public class RemarkData {
    private final Data data;
    private final DALNode inputNode;
    private final DataRemarkNode remarkNode;
    private final RuntimeContextBuilder.DALRuntimeContext runtimeContext;

    public RemarkData(Data data, DALNode inputNode, DALNode remarkNode, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        this.data = data;
        this.inputNode = inputNode;
        this.remarkNode = (DataRemarkNode) remarkNode;
        this.runtimeContext = runtimeContext;
    }

    public DALNode inputNode() {
        return inputNode;
    }

    public DataRemarkNode remarkNode() {
        return remarkNode;
    }

    public String remark() {
        return (String) remarkNode.evaluate(runtimeContext);
    }

    public RuntimeContextBuilder.DALRuntimeContext runtimeContext() {
        return runtimeContext;
    }

    public Data data() {
        return data;
    }
}
