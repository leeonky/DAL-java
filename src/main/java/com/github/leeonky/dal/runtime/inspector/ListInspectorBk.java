package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.util.TextUtil;
import com.github.leeonky.util.Classes;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;

@Deprecated
public class ListInspectorBk implements InspectorBk.Cacheable {

    @Override
    public String cachedInspect(Data data, InspectorContextBk context) {
        DumpingContext dumpingContext = context.dumpingContext();
        String s = type(data, context) + body(data.getDataList(), dumpingContext);
        return s;
    }

    //    TODO refactor
    private String body(List<Data> dataList, DumpingContext dumpingContext) {
        dumpingContext.append("[");
        AtomicInteger index = new AtomicInteger(0);
        DumpingContext indentContext = dumpingContext.indent();
        String collect = dataList.stream().map(subData -> {
                    InspectorContextBk subContextBk = indentContext.index(index.getAndIncrement());
                    DumpingContext subContext = subContextBk.dumpingContext();
                    subContext.newLine();
                    String dump = subContextBk.dump(subData);
                    indentContext.appendThen(",");
                    return dump;
                })
                .map(TextUtil::indent).collect(joining(",\n", "[\n", "\n]"));
        if (dumpingContext.hasContent()) {
            dumpingContext.newLine();
        }
        dumpingContext.append("]");
        String content = dumpingContext.content();
        return content;
    }

    protected String type(Data data, InspectorContextBk context) {
        if (data.getInstance() instanceof Iterable || data.getInstance() instanceof Stream
                || data.getInstance().getClass().isArray())
            return "";
        context.dumpingContext().append(Classes.getClassName(data.getInstance()));
        context.dumpingContext().appendThen(" ");
        return Classes.getClassName(data.getInstance()) + " ";
    }
}
