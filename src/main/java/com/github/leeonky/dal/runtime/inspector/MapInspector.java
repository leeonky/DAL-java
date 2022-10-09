package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.util.TextUtil;

import java.util.Set;

import static java.util.stream.Collectors.joining;

public class MapInspector extends CacheableInspector {
    public MapInspector(Data data) {
        super(data);
    }

    @Override
    public String cachedInspect(String path, InspectorCache cache) {
        Set<Object> fieldNames = getFieldNames();
        if (fieldNames.isEmpty())
            return "{}";
        return fieldNames.stream().map(o -> dumpEntry(path, cache, o)).map(TextUtil::indent)
                .collect(joining(",\n", "{\n", "\n}"));
    }

    private String dumpEntry(String path, InspectorCache cache, Object o) {
        Data value;
        try {
            value = data.getValue(o);
        } catch (Exception e) {
            return key(o) + ": *throw* " + e;
        }
        return key(o) + ": " + value.buildInspector().dump(path + "." + o, cache);
    }

    protected String key(Object o) {
        return String.valueOf(o);
    }

    protected Set<Object> getFieldNames() {
        return data.getFieldNames();
    }
}
