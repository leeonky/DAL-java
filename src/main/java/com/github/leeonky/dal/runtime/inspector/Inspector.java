package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

public interface Inspector {
    static Inspector defaultInspector(Data data) {
        if (data.isNull())
            return (path, cache) -> "null";
        return data.isList() ? new ListInspector(data) : new MapInspector(data);
    }

    String inspect(String path, InspectorCache inspectorCache);

    default String dump(String path, InspectorCache caches) {
        return inspect(path, caches);
    }
}
