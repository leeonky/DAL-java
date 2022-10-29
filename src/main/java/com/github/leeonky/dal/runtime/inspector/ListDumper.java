package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

import java.util.List;
import java.util.stream.Stream;

//    TODO refactor
public class ListDumper implements Dumper.Cacheable {

    @Override
    public void cachedInspect(Data data, DumpingContext context) {
        DumpingContext dumpingContext = context.dumpingContext();
        type(data, context.dumpingContext());
        body(data.getDataList(), dumpingContext);
    }

    private void body(List<Data> dataList, DumpingContext dumpingContext) {
        DumpingContext indentContext = dumpingContext.append("[").indent();
        for (int i = 0; i < dataList.size(); i++) {
            indentContext.index(i).newLine().dump(dataList.get(i));
            indentContext.appendThen(",");
        }
        dumpingContext.optionalNewLine().append("]");
    }

    protected void type(Data data, DumpingContext context) {
        if (!(data.getInstance() instanceof Iterable) && !(data.getInstance() instanceof Stream)
                && !data.getInstance().getClass().isArray()) {
            context.append(Classes.getClassName(data.getInstance()));
            context.appendThen(" ");
        }
    }
}
