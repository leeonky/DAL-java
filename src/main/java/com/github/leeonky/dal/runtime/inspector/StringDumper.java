package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

public class StringDumper extends ValueDumper {
    @Override
    protected void inspectValue(Data data, DumpingContext dumpingContext) {
        dumpingContext.append("<" + data.getInstance().toString().replace("\\", "\\\\").replace("\n", "\\n")
                .replace("\r", "\\r").replace("\t", "\\t").replace("\b", "\\b") + ">");
    }
}