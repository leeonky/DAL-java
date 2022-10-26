package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import static java.lang.String.format;

public class InspectorContext {
    private final String path;
    private final InspectorCache cache;
    private final DALRuntimeContext dalRuntimeContext;

    public InspectorContext(String path, InspectorCache cache, DALRuntimeContext dalRuntimeContext) {
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
    public DALRuntimeContext getDalRuntimeContext() {
        return dalRuntimeContext;
    }

    public String inspect(Data data) {
        return dalRuntimeContext.fetchInspector(data).inspect(data, this);
    }

    public String dump(Data data) {
        return dalRuntimeContext.fetchInspector(data).dump(data, this);
    }

    public InspectorContext index(int index) {
        return new InspectorContext(format("%s[%d]", path, index), cache, dalRuntimeContext);
    }

    public InspectorContext sub(Object property) {
        return new InspectorContext(format("%s.%s", path, property), cache, dalRuntimeContext);
    }
}
