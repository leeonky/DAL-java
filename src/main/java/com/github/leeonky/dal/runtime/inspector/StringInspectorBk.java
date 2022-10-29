package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

public class StringInspectorBk extends ValueInspectorBk {
    @Override
    public void inspectValue(Data data, InspectorContextBk.DumpingContext dumpingContext) {
        dumpingContext.append("<" + data.getInstance().toString().replace("\\", "\\\\").replace("\n", "\\n")
                .replace("\r", "\\r").replace("\t", "\\t").replace("\b", "\\b") + ">");
    }
}
