package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.util.TextUtil;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static com.github.leeonky.util.Classes.getClassName;
import static java.util.stream.Collectors.joining;

public interface Inspector {
    //    TODO refactor
    static Inspector defaultInspector(Data data) {
        if (data.isNull())
            return (path, cache) -> "null";
        if (data.isList()) {
            return new CacheableInspector(data) {
                @Override
                public String cachedInspect(String path, InspectorCache cache) {
                    List<Data> dataList = data.getDataList();
                    if (dataList.isEmpty())
                        return "[]";
                    AtomicInteger index = new AtomicInteger(0);
                    return dataList.stream().map(data1 -> data1.buildInspector().dump(path + "[" + index.getAndIncrement() + "]", cache))
                            .map(TextUtil::indent).collect(joining(",\n", "[\n", "\n]"));
                }
            };
        }
        return new CacheableInspector(data) {
            @Override
            public String cachedInspect(String path, InspectorCache cache) {
                String type = data.getInstance() instanceof Map ? "" : getClassName(data.getInstance()) + " ";
                Set<Object> fieldNames = data.getFieldNames();
                if (fieldNames.isEmpty())
                    return type + "{}";
                return type + fieldNames.stream().map(o -> {
                            Data value;
                            try {
                                value = data.getValue(o);
                            } catch (Exception e) {
                                return o + ": *throw* " + e;
                            }
                            return o + ": " + value.buildInspector().dump(path + "." + o, cache);
                        }).map(TextUtil::indent)
                        .collect(joining(",\n", "{\n", "\n}"));
            }
        };
    }

    String inspect(String path, InspectorCache inspectorCache);

    default String dump(String path, InspectorCache caches) {
        return inspect(path, caches);
    }
}
