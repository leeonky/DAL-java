package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

public class ValueInspectorBk implements InspectorBk {
    protected String inspectType(Data data) {
        return Classes.getClassName(data.getInstance());
    }

    protected void inspectValue(Data data, DumpingContext dumpingContext) {
        dumpingContext.append("<" + data.getInstance().toString() + ">");
    }

    @Override
    public String inspect(Data data, InspectorContextBk context) {
        new ValueDumper().dumpDetail(data, context.dumpingContext());
        return "";
    }

    @Override
    public String dump(Data data, InspectorContextBk context) {
        new ValueDumper().dump(data, context.dumpingContext());
        return "";
    }
}
