package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

import java.util.Map;
import java.util.Set;

public class MapDumper implements Dumper.Cacheable {

    @Override
    public void cachedInspect(Data data, DumpingBuffer context) {
        dumpType(data, context);
        dumpBody(data, context);
    }

    private void dumpBody(Data data, DumpingBuffer dumpingBuffer) {
        DumpingBuffer indentContext = dumpingBuffer.append("{").indent();
        getFieldNames(data).forEach(fieldName -> {
            dumpField(data, fieldName, indentContext.sub(fieldName).newLine());
            indentContext.appendThen(",");
        });
        dumpingBuffer.optionalNewLine().append("}");
    }

    protected void dumpField(Data data, Object field, DumpingBuffer context) {
        Data value;
        context.append(key(field)).append(": ");
        try {
            value = data.getValue(field);
        } catch (Exception e) {
            context.append("*throw* " + e);
            return;
        }
        context.dumpValue(value);
    }

    protected String key(Object o) {
        return String.valueOf(o);
    }

    protected Set<Object> getFieldNames(Data data) {
        return data.fieldNames();
    }

    protected void dumpType(Data data, DumpingBuffer dumpingBuffer) {
        if (!(data.instance() instanceof Map))
            dumpingBuffer.append(Classes.getClassName(data.instance())).appendThen(" ");
    }
}
