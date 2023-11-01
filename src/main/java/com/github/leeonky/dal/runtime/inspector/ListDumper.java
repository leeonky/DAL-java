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
        dumpingBuffer.append("[").indent(indentBuffer ->
                data.list().wraps().forEach(ie -> {
                    indentBuffer.index(ie.index()).newLine().dumpValue(ie.value());
                    indentBuffer.appendThen(",");
                })).optionalNewLine().append("]");
    }

    protected void dumpType(Data data, DumpingBuffer context) {
        if (!(data.instance() instanceof Iterable) && !(data.instance() instanceof Stream)
                && !data.instance().getClass().isArray())
            context.append(Classes.getClassName(data.instance())).appendThen(" ");
    }
}
