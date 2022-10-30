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

    @Deprecated
    public String act(String path, Data data, Supplier<String> action, DumpingContext dumpingContext) {
        InspectorCacheKey key = new InspectorCacheKey(data);
        String reference = caches.get(key);
        if (reference == null) {
            caches.put(key, path);
            return action.get();
        }
        dumpingContext.append("*reference* " + reference);
        return "*reference* " + reference;
    }

    public void act(String path, Data data, DumpingContext context, Runnable runnable) {
        InspectorCacheKey key = new InspectorCacheKey(data);
        String reference = caches.get(key);
        if (reference == null) {
            caches.put(key, path);
            runnable.run();
            return;
        }
        context.append("*reference* " + reference);
    }
}
