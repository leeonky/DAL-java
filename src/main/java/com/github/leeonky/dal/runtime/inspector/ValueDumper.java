package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

public class ValueDumper implements Dumper {
    protected void inspectType(Data data, DumpingBuffer dumpingBuffer) {
        dumpingBuffer.append(Classes.getClassName(data.instance()));
    }

    protected void inspectValue(Data data, DumpingBuffer dumpingBuffer) {
        dumpingBuffer.append("<" + data.instance().toString() + ">");
    }

    @Override
    public void dump(Data data, DumpingBuffer dumpingBuffer) {
        inspectType(data, dumpingBuffer);
        dumpingBuffer.newLine();
        inspectValue(data, dumpingBuffer);
    }

    @Override
    public void dumpValue(Data data, DumpingBuffer dumpingBuffer) {
        inspectType(data, dumpingBuffer);
        dumpingBuffer.appendThen(" ");
        inspectValue(data, dumpingBuffer);
    }
}
