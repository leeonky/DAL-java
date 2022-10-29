package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.util.TextUtil;
import com.github.leeonky.util.Classes;

import java.util.Map;
import java.util.Set;

import static java.util.stream.Collectors.joining;

public class MapInspectorBk implements InspectorBk.Cacheable {

    @Override
    public String cachedInspect(Data data, InspectorContextBk context) {
        Set<Object> fieldNames = getFieldNames(data);
        InspectorContextBk.DumpingContext dumpingContext = context.dumpingContext();
        return type(data, dumpingContext) + getString(data, context, fieldNames, dumpingContext);
    }

    private String getString(Data data, InspectorContextBk context, Set<Object> fieldNames, InspectorContextBk.DumpingContext dumpingContext) {
        dumpingContext.append("{");
        if (fieldNames.isEmpty()) {
            dumpingContext.append("}");
            return "{}";
        }
        InspectorContextBk.DumpingContext indentContext = dumpingContext.indent(1);
        String collect = fieldNames.stream()
                .map(fieldName -> {
                    context.setDumpingContext(indentContext);
                    InspectorContextBk subContextBk = context.sub(fieldName);

                    InspectorContextBk.DumpingContext subContext = subContextBk.dumpingContext();
                    subContext.newLine();
                    subContext.append(key(fieldName));
                    subContext.append(": ");
                    String s = key(fieldName) + ": " + dumpField(data, fieldName, subContextBk);
                    indentContext.appendThen(",");

                    return s;
                })
                .map(TextUtil::indent).collect(joining(",\n", "{\n", "\n}"));

        if (dumpingContext.hasContent()) {
            dumpingContext.newLine();
        }
        dumpingContext.append("}");
        return collect;
    }

    protected String dumpField(Data data, Object field, InspectorContextBk sub) {
        Data value;
        try {
            value = data.getValue(field);
        } catch (Exception e) {
            sub.dumpingContext().append("*throw* " + e);
            return "*throw* " + e;
        }
        return sub.dump(value);
    }

    protected String key(Object o) {
        return String.valueOf(o);
    }

    protected Set<Object> getFieldNames(Data data) {
        return data.getFieldNames();
    }

    protected String type(Data data, InspectorContextBk.DumpingContext dumpingContext) {
        if (data.getInstance() instanceof Map)
            return "";
        dumpingContext.append(Classes.getClassName(data.getInstance()));
        dumpingContext.appendThen(" ");
        return Classes.getClassName(data.getInstance()) + " ";
    }
}
