package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.InvocationException;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

public class MetaData {
    private final DALNode metaDataNode;
    private final DALNode symbolNode;
    private final DALRuntimeContext runtimeContext;
    private Data data;
    private Throwable error;
    private RuntimeException originalException;

    public MetaData(DALNode metaDataNode, DALNode symbolNode, DALRuntimeContext runtimeContext) {
        this.metaDataNode = metaDataNode;
        this.symbolNode = symbolNode;
        this.runtimeContext = runtimeContext;
        setData(() -> getMetaDataNode().evaluateData(getRuntimeContext()));
    }

    private void setData(Supplier<Data> supplier) {
        try {
            data = supplier.get();
        } catch (RuntimeException e) {
            if (!(e.getCause() instanceof InvocationException))
                throw e;
            originalException = e;
            error = e.getCause().getCause();
            data = runtimeContext.wrap(null);
        }
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
        if (error != null)
            throw originalException;
        return data;
    }

    public Throwable catchError() {
        Throwable throwable = error;
        error = null;
        return throwable;
    }

    private final List<Class<?>> callTypes = new ArrayList<>();

    public Object callSuper() {
        return getRuntimeContext().fetchSuperMetaFunction(symbolNode.getRootSymbolName(), this)
                .orElseThrow(this::noSuperError).apply(this);
    }

    public Object callSuper(Supplier<Object> supplier) {
        setData(() -> {
            Object newData = supplier.get();
            checkType(newData);
            return runtimeContext.wrap(newData);
        });
        return callSuper();
    }

    private void checkType(Object data) {
        Class<?> expect = this.data.getInstance().getClass();
        Class<?> actual = Objects.requireNonNull(data).getClass();
        if (actual.isAnonymousClass())
            actual = actual.getSuperclass();
        if (!actual.equals(expect))
            throw new RuntimeException(String.format("Do not allow change data type in callSuper, expect %s but %s",
                    expect.getName(), actual.getName()), symbolNode.getPositionBegin());
    }

    private RuntimeException noSuperError() {
        return new RuntimeException(String.format("Local meta property `%s` has no super in type %s",
                symbolNode.getRootSymbolName(), callTypes.get(callTypes.size() - 1).getName()),
                symbolNode.getPositionBegin());
    }

    public void addCallType(Class<?> callType) {
        callTypes.add(callType);
    }

    public boolean calledBy(Class<?> type) {
        return callTypes.contains(type);
    }

    public boolean isInstance(Class<?> type) {
        return type.isInstance(data.getInstance());
    }
}
