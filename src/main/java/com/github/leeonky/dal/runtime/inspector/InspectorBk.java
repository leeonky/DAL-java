package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

@Deprecated
public interface InspectorBk {
    static InspectorBk defaultInspectorBk(Data data, DALRuntimeContext dalRuntimeContext) {
        Inspector inspector = dalRuntimeContext.fetchInspector(data);
        return new InspectorBk() {
            @Override
            public String inspect(String path, InspectorCache cache) {
                return inspector.inspect(data, new InspectorContext(path, cache, data.context));
            }

            @Override
            public String dump(String path, InspectorCache cache) {
                return inspector.dump(data, new InspectorContext(path, cache, data.context));
            }
        };
    }

    String inspect(String path, InspectorCache inspectorCache);

    default String dump(String path, InspectorCache caches) {
        return inspect(path, caches);
    }
}
