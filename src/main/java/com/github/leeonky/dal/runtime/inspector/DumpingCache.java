package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

public class DumpingCache {
    private final Map<DumpingCacheKey, String> caches = new HashMap<>();

    private DumpingCache() {
    }

    public static DumpingCache cache() {
        return new DumpingCache();
    }

    @Deprecated
    public String act(String path, Data data, Supplier<String> action, DumpingContext dumpingContext) {
        DumpingCacheKey key = new DumpingCacheKey(data);
        String reference = caches.get(key);
        if (reference == null) {
            caches.put(key, path);
            return action.get();
        }
        dumpingContext.append("*reference* ").append(reference);
        return "*reference* " + reference;
    }

    public void act(String path, Data data, DumpingContext context, Runnable runnable) {
        DumpingCacheKey key = new DumpingCacheKey(data);
        String reference = caches.get(key);
        if (reference == null) {
            caches.put(key, path);
            runnable.run();
            return;
        }
        context.append("*reference* " + reference);
    }
}
