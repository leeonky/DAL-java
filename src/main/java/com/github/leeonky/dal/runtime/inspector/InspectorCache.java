package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BiFunction;

public class InspectorCache {
    private final Map<InspectorCacheKey, String> caches = new HashMap<>();

    private InspectorCache() {
    }

    public static InspectorCache cache() {
        return new InspectorCache();
    }

    public String act(String path, BiFunction<String, InspectorCache, String> action, Data data) {
        InspectorCacheKey key = new InspectorCacheKey(data);
        String reference = caches.get(key);
        if (reference == null) {
            caches.put(key, path);
            return action.apply(path, this);
        }
        return "*reference* " + reference;
    }
}
