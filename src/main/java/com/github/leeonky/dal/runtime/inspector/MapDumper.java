package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

import java.util.Map;
import java.util.Set;

//TODO refactor
public class MapDumper implements Dumper.Cacheable {

    @Override
    public void cachedInspect(Data data, DumpingContext context) {
        type(data, context);
        body(data, context);
    }

    private void body(Data data, DumpingContext dumpingContext) {
        dumpingContext.append("{");
        DumpingContext indentContext = dumpingContext.indent();
        getFieldNames(data).forEach(fieldName -> {
            DumpingContext sub = indentContext.sub(fieldName);
            dumpField(data, fieldName, sub);
            indentContext.appendThen(",");
        });
        if (dumpingContext.hasContent())
            dumpingContext.newLine();
        dumpingContext.append("}");
    }

    protected void dumpField(Data data, Object field, DumpingContext context) {
        Data value;
        context.newLine().append(key(field)).append(": ");
        try {
            value = data.getValue(field);
        } catch (Exception e) {
            context.append("*throw* " + e);
            return;
        }
        context.dump(value);
    }

    protected String key(Object o) {
        return String.valueOf(o);
    }

    protected Set<Object> getFieldNames(Data data) {
        return data.getFieldNames();
    }

    protected String type(Data data, DumpingContext dumpingContext) {
        if (data.getInstance() instanceof Map)
            return "";
        dumpingContext.append(Classes.getClassName(data.getInstance()));
        dumpingContext.appendThen(" ");
        return Classes.getClassName(data.getInstance()) + " ";
    }
}
