package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

import static java.lang.String.format;

public class ValueInspectorBk implements InspectorBk {
    protected String inspectType(Data data) {
        return Classes.getClassName(data.getInstance());
    }

    protected String inspectValue(Data data) {
        return "<" + data.getInstance().toString() + ">";
    }

    @Override
    public String inspect(Data data, InspectorContextBk context) {
        return format("%s\n%s", inspectType(data), inspectValue(data)).trim();
    }

    @Override
    public String dump(Data data, InspectorContextBk context) {
        return format("%s %s", inspectType(data), inspectValue(data)).trim();
    }
}
