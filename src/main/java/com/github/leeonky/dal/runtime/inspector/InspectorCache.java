package com.github.leeonky.dal.runtime.inspector;

import java.util.Map;

public class InspectorCache {
    private final Map<InspectorCacheKey, String> caches;

    @Deprecated
    public InspectorCache(Map<InspectorCacheKey, String> caches) {
        this.caches = caches;
    }

    public Map<InspectorCacheKey, String> getCaches() {
        return caches;
    }
}
