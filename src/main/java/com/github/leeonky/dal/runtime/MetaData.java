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
    private Object name;

    public MetaData(DALNode metaDataNode, DALNode symbolNode, DALRuntimeContext runtimeContext) {
        this.metaDataNode = metaDataNode;
        this.symbolNode = symbolNode;
        this.runtimeContext = runtimeContext;
        name = symbolNode.getRootSymbolName();
        setData(() -> getMetaDataNode().evaluateData(getRuntimeContext()));
    }

    public MetaData(DALNode metaDataNode, DALNode symbolNode, DALRuntimeContext runtimeContext,
                    Data data, Throwable error, RuntimeException originalException, String name) {
        this.metaDataNode = metaDataNode;
        this.symbolNode = symbolNode;
        this.runtimeContext = runtimeContext;
        this.name = name;
        this.data = data;
        this.error = error;
        this.originalException = originalException;
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
        return getRuntimeContext().fetchSuperMetaFunction(this)
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

    public Object callGlobal() {
        return getRuntimeContext().fetchGlobalMetaFunction(this).apply(this);
    }

    public Object callGlobal(Supplier<Object> supplier) {
        setData(() -> runtimeContext.wrap(supplier.get()));
        return callGlobal();
    }

    public MetaData newMeta(String name) {
        return new MetaData(metaDataNode, symbolNode, runtimeContext, data, error, originalException, name);
    }

    public Object callMeta(String another) {
        MetaData metaData = newMeta(another);
        return getRuntimeContext().fetchGlobalMetaFunction(metaData).apply(metaData);
    }

    public Object callMeta(String another, Supplier<Object> supplier) {
        MetaData metaData = newMeta(another);
        metaData.setData(() -> runtimeContext.wrap(supplier.get()));
        return getRuntimeContext().fetchGlobalMetaFunction(metaData).apply(metaData);
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

    public Object getName() {
        return name;
    }
}
