package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

public abstract class CacheableInspector implements Inspector {
    protected final Data data;

    public CacheableInspector(Data data) {
        this.data = data;
    }

    @Override
    public String dump(String path, InspectorCache cache) {
        return cache.act(path, this::cachedDump, data);
    }

    @Override
    public String inspect(String path, InspectorCache cache) {
        return cache.act(path, this::cachedInspect, data);
    }

    public String cachedDump(String path, InspectorCache cache) {
        return cachedInspect(path, cache);
    }

    public abstract String cachedInspect(String path, InspectorCache cache);
}
