package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.util.TextUtil;
import com.github.leeonky.util.Classes;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;

public class ListInspectorBk implements InspectorBk.Cacheable {

    @Override
    public String cachedInspect(Data data, InspectorContextBk context) {
        InspectorContextBk.DumpingContext dumpingContext = context.dumpingContext();
        String s = type(data, context) + body(context, data.getDataList(), dumpingContext);
        return s;
    }

    //    TODO refactor
    private String body(InspectorContextBk context, List<Data> dataList, InspectorContextBk.DumpingContext dumpingContext) {
        dumpingContext.append("[");
//        if (dataList.isEmpty()) {
//            dumpingContext.append("]");
//            return dumpingContext.content();
//        }
//        [\n\ta,\n\tb,\n\tb,\n\tb\n\t]
//        InspectorContextBk.DumpingContext subContext = dumpingContext.newLine().subContext();
//        for (int i = 0; i < dataList.size(); i++) {
//            subContext.append(context.index(i).dump(dataList.get(i)));
//        }
//        InspectorContextBk.DumpingContext subContext = dumpingContext.indent();
        AtomicInteger index = new AtomicInteger(0);
        InspectorContextBk.DumpingContext indentContext = dumpingContext.indent(1);
        String collect = dataList.stream().map(subData -> {
                    context.setDumpingContext(indentContext);
                    InspectorContextBk subContextBk = context.index(index.getAndIncrement());
                    InspectorContextBk.DumpingContext subContext = subContextBk.dumpingContext();
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
