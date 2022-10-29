package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

public class ValueInspectorBk implements InspectorBk {
    protected String inspectType(Data data) {
        return Classes.getClassName(data.getInstance());
    }

    protected void inspectValue(Data data, InspectorContextBk.DumpingContext dumpingContext) {
        dumpingContext.append("<" + data.getInstance().toString() + ">");
    }

    @Override
    public String inspect(Data data, InspectorContextBk context) {
        InspectorContextBk.DumpingContext dumpingContext = context.dumpingContext();
        dumpingContext.append(inspectType(data));
        dumpingContext.newLine();
        inspectValue(data, dumpingContext);
        return dumpingContext.content();
    }

    @Override
    public String dump(Data data, InspectorContextBk context) {
        InspectorContextBk.DumpingContext dumpingContext = context.dumpingContext();
        dumpingContext.append(inspectType(data));
        dumpingContext.appendThen(" ");
        inspectValue(data, dumpingContext);
        return dumpingContext.content();
    }
}
