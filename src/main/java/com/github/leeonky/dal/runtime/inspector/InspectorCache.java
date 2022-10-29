package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

public class InspectorCache {
    private final Map<InspectorCacheKey, String> caches = new HashMap<>();

    private InspectorCache() {
    }

    public static InspectorCache cache() {
        return new InspectorCache();
    }

    public String act(String path, Data data, Supplier<String> action, InspectorContextBk inspectorContextBk) {
        InspectorCacheKey key = new InspectorCacheKey(data);
        String reference = caches.get(key);
        if (reference == null) {
            caches.put(key, path);
            return action.get();
        }
        inspectorContextBk.dumpingContext().append("*reference* " + reference);
        return "*reference* " + reference;
    }
}
