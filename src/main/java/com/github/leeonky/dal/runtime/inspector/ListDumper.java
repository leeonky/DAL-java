package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

import java.util.stream.Stream;

public class ListDumper implements Dumper.Cacheable {

    @Override
    public void cachedInspect(Data data, DumpingBuffer context) {
        dumpType(data, context);
        dumpBody(data, context);
    }

    private void dumpBody(Data data, DumpingBuffer dumpingBuffer) {
        DumpingBuffer indentContext = dumpingBuffer.append("[").indent();
        data.indexedListData().forEach(ie -> {
            indentContext.index(ie.index()).newLine().dumpValue(ie.value());
            indentContext.appendThen(",");
        });
        dumpingBuffer.optionalNewLine().append("]");
    }

    protected void dumpType(Data data, DumpingBuffer context) {
        if (!(data.getInstance() instanceof Iterable) && !(data.getInstance() instanceof Stream)
                && !data.getInstance().getClass().isArray())
            context.append(Classes.getClassName(data.getInstance())).appendThen(" ");
    }
}
