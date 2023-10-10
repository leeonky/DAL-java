package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.InvocationException;

public class MetaData {
    private final DALNode metaDataNode;
    private final DALNode symbolNode;
    private final DALRuntimeContext runtimeContext;
    private Data cachedData = null;
    private Throwable error;

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
        if (cachedData == null)
            try {
                return cachedData = getMetaDataNode().evaluateData(getRuntimeContext());
            } catch (RuntimeException e) {
                if (!(e.getCause() instanceof InvocationException))
                    throw e;
                error = e.getCause().getCause();
            }
        return cachedData;
    }

    public Throwable getError() {
        return error;
    }
}
