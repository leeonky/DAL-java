package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.util.TextUtil;

import java.util.Map;
import java.util.Set;

import static com.github.leeonky.util.Classes.getClassName;
import static java.util.stream.Collectors.joining;

public class MapInspector extends CacheableInspector {

    public MapInspector(Data data) {
        super(data);
    }

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
}
