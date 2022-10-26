package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class InspectorContext {
    private final String path;
    private final InspectorCache cache;
    private final RuntimeContextBuilder.DALRuntimeContext dalRuntimeContext;

    public InspectorContext(String path, InspectorCache cache, RuntimeContextBuilder.DALRuntimeContext dalRuntimeContext) {
        this.path = path;
        this.cache = cache;
        this.dalRuntimeContext = dalRuntimeContext;
    }

    //    TODO private
    @Deprecated
    public String getPath() {
        return path;
    }

    //    TODO private
    @Deprecated
    public InspectorCache getCache() {
        return cache;
    }

    @Deprecated
    public RuntimeContextBuilder.DALRuntimeContext getDalRuntimeContext() {
        return dalRuntimeContext;
    }
}
