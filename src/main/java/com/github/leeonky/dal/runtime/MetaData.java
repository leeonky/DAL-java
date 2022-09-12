package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class MetaData {
    private final DALNode metaDataNode;
    private final DALNode symbolNode;
    private final DALRuntimeContext runtimeContext;

    public MetaData(DALNode metaDataNode, DALNode symbolNode, DALRuntimeContext runtimeContext) {
        this.metaDataNode = metaDataNode;
        this.symbolNode = symbolNode;
        this.runtimeContext = runtimeContext;
    }

    public DALNode getMetaDataNode() {
        return metaDataNode;
    }

    public DALNode getSymbolNode() {
        return symbolNode;
    }

    public DALRuntimeContext getRuntimeContext() {
        return runtimeContext;
    }

    public Data evaluateInput() {
        return getMetaDataNode().evaluateData(getRuntimeContext());
    }
}
