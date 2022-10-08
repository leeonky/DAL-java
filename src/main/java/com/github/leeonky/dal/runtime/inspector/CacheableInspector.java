package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

public abstract class CacheableInspector implements Inspector {
    private final Data data;

    public CacheableInspector(Data data) {
        this.data = data;
    }

    @Override
    public String dump(String path, InspectorCache cache) {
        InspectorCacheKey key = new InspectorCacheKey(data);
        String reference = cache.getCaches().get(key);
        if (reference == null) {
            cache.getCaches().put(key, path);
            return cachedDump(path, cache);
        }
        return "*reference* " + reference;
    }

    @Override
    public String inspect(String path, InspectorCache cache) {
        InspectorCacheKey key = new InspectorCacheKey(data);
        String reference = cache.getCaches().get(key);
        if (reference == null) {
            cache.getCaches().put(key, path);
            return cachedInspect(path, cache);
        }
        return "*reference* " + reference;
    }

    public String cachedDump(String path, InspectorCache cache) {
        return cachedInspect(path, cache);
    }

    public abstract String cachedInspect(String path, InspectorCache cache);
}
