package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

@Deprecated
public interface InspectorBk {
    InspectorBk VALUE_INSPECTOR_BK = new ValueInspectorBk(),
            STRING_INSPECTOR_BK = new StringInspectorBk(),
            MAP_INSPECTOR_BK = new MapInspectorBk(),
            LIST_INSPECTOR_BK = new ListInspectorBk();

    String inspect(Data data, InspectorContextBk context);

    default String dump(Data data, InspectorContextBk context) {
        return inspect(data, context);
    }

    interface Cacheable extends InspectorBk {

        @Override
        default String dump(Data data, InspectorContextBk context) {
            return context.cached(data, () -> cachedDump(data, context));
        }

        @Override
        default String inspect(Data data, InspectorContextBk context) {
            return context.cached(data, () -> cachedInspect(data, context));
        }

        default String cachedDump(Data data, InspectorContextBk context) {
            return cachedInspect(data, context);
        }

        String cachedInspect(Data data, InspectorContextBk context);
    }
}
