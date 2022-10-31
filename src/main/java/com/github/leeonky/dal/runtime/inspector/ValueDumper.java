package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

public class ValueDumper implements Dumper {
    protected void inspectType(Data data, DumpingContext dumpingContext) {
        dumpingContext.append(Classes.getClassName(data.getInstance()));
    }

    protected void inspectValue(Data data, DumpingContext dumpingContext) {
        dumpingContext.append("<" + data.getInstance().toString() + ">");
    }

    @Override
    public void dump(Data data, DumpingContext dumpingContext) {
        inspectType(data, dumpingContext);
        dumpingContext.newLine();
        inspectValue(data, dumpingContext);
    }

    @Override
    public void dumpValue(Data data, DumpingContext dumpingContext) {
        inspectType(data, dumpingContext);
        dumpingContext.appendThen(" ");
        inspectValue(data, dumpingContext);
    }
}
