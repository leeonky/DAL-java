package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.InvocationException;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

public class MetaData extends RuntimeData {
    private Throwable error;
    private RuntimeException originalException;
    private final Object name;
    protected Data data;

    public MetaData(DALNode inputNode, DALNode operandNode, DALRuntimeContext runtimeContext) {
        super(null, inputNode, operandNode, runtimeContext, null);
        name = operandNode.getRootSymbolName();
        setData(() -> inputNode().evaluateData(runtimeContext()));
    }

    private MetaData(DALNode inputNode, DALNode operandNode, DALRuntimeContext runtimeContext,
                     Data data, Throwable error, RuntimeException originalException, String name) {
        super(null, inputNode, operandNode, runtimeContext, null);
        this.name = name;
        this.error = error;
        this.originalException = originalException;
        this.data = data;
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

    public Throwable catchError() {
        Throwable throwable = error;
        error = null;
        return throwable;
    }

    private final List<Class<?>> callTypes = new ArrayList<>();

    public Object callSuper() {
        return runtimeContext().fetchSuperMetaFunction(this)
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
        return runtimeContext().fetchGlobalMetaFunction(this).apply(this);
    }

    public Object callGlobal(Supplier<Object> supplier) {
        setData(() -> runtimeContext.wrap(supplier.get()));
        return callGlobal();
    }

    private MetaData newMeta(String name) {
        return new MetaData(inputNode, OperandNode, runtimeContext, data, error, originalException, name);
    }

    public Object callMeta(String another) {
        MetaData metaData = newMeta(another);
        return runtimeContext().fetchGlobalMetaFunction(metaData).apply(metaData);
    }

    public Object callMeta(String another, Supplier<Object> supplier) {
        MetaData metaData = newMeta(another);
        metaData.setData(() -> runtimeContext.wrap(supplier.get()));
        return runtimeContext().fetchGlobalMetaFunction(metaData).apply(metaData);
    }

    private void checkType(Object data) {
        Class<?> expect = this.data.instance().getClass();
        Class<?> actual = Objects.requireNonNull(data).getClass();
        if (actual.isAnonymousClass())
            actual = actual.getSuperclass();
        if (!actual.equals(expect))
            throw new RuntimeException(String.format("Do not allow change data type in callSuper, expect %s but %s",
                    expect.getName(), actual.getName()), OperandNode.getPositionBegin());
    }

    private RuntimeException noSuperError() {
        return new RuntimeException(String.format("Local meta property `%s` has no super in type %s",
                OperandNode.getRootSymbolName(), callTypes.get(callTypes.size() - 1).getName()),
                OperandNode.getPositionBegin());
    }

    public void addCallType(Class<?> callType) {
        callTypes.add(callType);
    }

    public boolean calledBy(Class<?> type) {
        return callTypes.contains(type);
    }

    public boolean isInstance(Class<?> type) {
        return type.isInstance(data.instance());
    }

    public Object name() {
        return name;
    }

    @Override
    public Data data() {
        if (error != null)
            throw originalException;
        return data;
    }
}
