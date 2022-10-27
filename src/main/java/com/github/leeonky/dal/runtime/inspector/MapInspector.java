package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.util.TextUtil;
import com.github.leeonky.util.Classes;

import java.util.Map;
import java.util.Set;

import static java.util.stream.Collectors.joining;

public class MapInspector implements Inspector.Cacheable {

    @Override
    public String cachedInspect(Data data, InspectorContext context) {
        Set<Object> fieldNames = getFieldNames(data);
        return type(data) + (fieldNames.isEmpty() ? "{}" : fieldNames.stream()
                .map(fieldName -> dumpEntry(key(fieldName), dumpField(data, fieldName, context)))
                .map(TextUtil::indent).collect(joining(",\n", "{\n", "\n}")));
    }

    protected String dumpEntry(String key, String value) {
        return key + ": " + value;
    }

    protected String dumpField(Data data, Object field, InspectorContext context) {
        Data value;
        try {
            value = data.getValue(field);
        } catch (Exception e) {
            return "*throw* " + e;
        }
        return context.sub(field).dump(value);
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
