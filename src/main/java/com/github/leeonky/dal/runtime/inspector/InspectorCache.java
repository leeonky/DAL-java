package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Supplier;

public class InspectorCache {
    private final Map<InspectorCacheKey, String> caches = new HashMap<>();

    private InspectorCache() {
    }

    public static InspectorCache cache() {
        return new InspectorCache();
    }

    @Deprecated
    public String act(String path, BiFunction<String, InspectorCache, String> action, Data data) {
        InspectorCacheKey key = new InspectorCacheKey(data);
        String reference = caches.get(key);
        if (reference == null) {
            caches.put(key, path);
            return action.apply(path, this);
        }
        return "*reference* " + reference;
    }

    public String act(InspectorContext context, BiFunction<Data, InspectorContext, String> action, Data data) {
        InspectorCacheKey key = new InspectorCacheKey(data);
        String reference = caches.get(key);
        if (reference == null) {
            caches.put(key, context.getPath());
            return action.apply(data, context);
        }
        return "*reference* " + reference;
    }


    public String act(String path, Data data, Supplier<String> action) {
        InspectorCacheKey key = new InspectorCacheKey(data);
        String reference = caches.get(key);
        if (reference == null) {
            caches.put(key, path);
            return action.get();
        }
        return "*reference* " + reference;
    }
}
