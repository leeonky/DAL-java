package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.function.Supplier;

import static java.lang.String.format;

@Deprecated
public class InspectorContextBk {
    //    TODO private
    final String path;
    final InspectorCache cache;
    final DALRuntimeContext dalRuntimeContext;

    protected InspectorContextBk(String path, InspectorCache cache, DALRuntimeContext dalRuntimeContext) {
        this.path = path;
        this.cache = cache;
        this.dalRuntimeContext = dalRuntimeContext;
    }

    public String getPath() {
        return path;
    }

    public DALRuntimeContext getDalRuntimeContext() {
        return dalRuntimeContext;
    }

    public String inspect(Data data) {
        return dalRuntimeContext.fetchInspector(data).inspect(data, this);
    }

    @Deprecated
    public String dump(Data data) {
        return dalRuntimeContext.fetchInspector(data).dump(data, this);
    }

    public DumpingContext index(int index) {
        DumpingContext dumpingContext = new DumpingContext(format("%s[%d]", path, index), cache, dalRuntimeContext);
        dumpingContext.stringBuilder = dumpingContext().stringBuilder;
        dumpingContext.indent = dumpingContext().indent;

        dumpingContext.thens = dumpingContext().thens;
        dumpingContext().thens = new StringBuilder();
        return dumpingContext;
    }

    public DumpingContext sub(Object property) {
        DumpingContext dumpingContext = new DumpingContext(format("%s.%s", path, property), cache, dalRuntimeContext);
        dumpingContext.stringBuilder = dumpingContext().stringBuilder;
        dumpingContext.indent = dumpingContext().indent;

        dumpingContext.thens = dumpingContext().thens;
        dumpingContext().thens = new StringBuilder();

        return dumpingContext;
    }

    public String cached(Data data, Supplier<String> action) {
        return cache.act(path, data, action, this);
    }

    public void cached(Data data, Runnable runnable) {
        cache.act(path, data, dumpingContext(), runnable);
    }

    @Deprecated
    public DumpingContext dumpingContext() {
        return (DumpingContext) this;
    }
}
