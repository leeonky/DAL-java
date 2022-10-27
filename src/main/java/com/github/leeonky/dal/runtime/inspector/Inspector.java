package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

public interface Inspector {
    Inspector VALUE_INSPECTOR = new ValueInspector(),
            STRING_INSPECTOR = new StringInspector(),
            MAP_INSPECTOR = new MapInspector(),
            LIST_INSPECTOR = new ListInspector();

    String inspect(Data data, InspectorContext context);

    default String dump(Data data, InspectorContext context) {
        return inspect(data, context);
    }

    interface Cacheable extends Inspector {

        @Override
        default String dump(Data data, InspectorContext context) {
            return context.cached(data, () -> cachedDump(data, context));
        }

        @Override
        default String inspect(Data data, InspectorContext context) {
            return context.cached(data, () -> cachedInspect(data, context));
        }

        default String cachedDump(Data data, InspectorContext context) {
            return cachedInspect(data, context);
        }

        String cachedInspect(Data data, InspectorContext context);
    }
}
