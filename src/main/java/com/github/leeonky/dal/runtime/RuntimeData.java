package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class RuntimeData {
    protected final DALRuntimeContext runtimeContext;
    protected final Data data;

    public RuntimeData(Data data, DALRuntimeContext runtimeContext) {
        this.runtimeContext = runtimeContext;
        this.data = data;
    }

    public DALRuntimeContext runtimeContext() {
        return runtimeContext;
    }

    public Data data() {
        return data;
    }
}
