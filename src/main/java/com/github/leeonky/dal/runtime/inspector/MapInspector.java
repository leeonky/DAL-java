package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.util.TextUtil;
import com.github.leeonky.util.Classes;

import java.util.Map;
import java.util.Set;

import static java.util.stream.Collectors.joining;

public class MapInspector implements Inspector.Cacheable {

    public static final MapInspector INSTANCE = new MapInspector();

    @Override
    public String cachedInspect(Data data, InspectorContext context) {
        String type = type(data);
        Set<Object> fieldNames = getFieldNames(data);
        if (fieldNames.isEmpty())
            return type + "{}";
        return type + fieldNames.stream().map(fieldName -> dumpEntry(data, context.getPath(), context.getCache(), fieldName)).map(TextUtil::indent)
                .collect(joining(",\n", "{\n", "\n}"));
    }

    private String dumpEntry(Data data, String path, InspectorCache cache, Object o) {
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

    protected Set<Object> getFieldNames(Data data) {
        return data.getFieldNames();
    }

    protected String type(Data data) {
        return data.getInstance() instanceof Map ? "" : Classes.getClassName(data.getInstance()) + " ";
    }
}
