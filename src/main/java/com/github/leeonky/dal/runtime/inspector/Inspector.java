package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

//TODO refactor
public interface Inspector {
    String inspect(Data data, InspectorContext context);

    default String dump(Data data, InspectorContext context) {
        return inspect(data, context);
    }

    interface Cacheable extends Inspector {

        @Override
        default String dump(Data data, InspectorContext context) {
            return context.getCache().act(context.getPath(), data, () -> cachedDump(data, context));
        }

        @Override
        default String inspect(Data data, InspectorContext context) {
            return context.getCache().act(context.getPath(), data, () -> cachedInspect(data, context));
        }

        default String cachedDump(Data data, InspectorContext context) {
            return cachedInspect(data, context);
        }

        String cachedInspect(Data data, InspectorContext context);
    }
}
