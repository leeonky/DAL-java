package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.InvocationException;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

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

    public Data getData() {
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

    private final List<Class<?>> callTypes = new ArrayList<>();

    public Object callSuper() {
        return getRuntimeContext().fetchSuperMetaFunction(symbolNode.getRootSymbolName(), this)
                .orElseThrow(this::noSuperError).apply(this);
    }

    public Object callSuper(Object data) {
        cachedData = runtimeContext.wrap(data);
        return callSuper();
    }

    private RuntimeException noSuperError() {
        return new RuntimeException(String.format("Local meta property `%s` has no super in type %s\n  %s",
                symbolNode.getRootSymbolName(), callTypes.get(callTypes.size() - 1).getName(),
                callTypes.stream().map(Class::getName).collect(Collectors.joining(" => "))),
                symbolNode.getPositionBegin());
    }

    void addCallType(Class<?> callType) {
        callTypes.add(callType);
    }

    boolean calledBy(Class<?> type) {
        return callTypes.contains(type);
    }
}
