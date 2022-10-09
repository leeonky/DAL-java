package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.util.TextUtil;
import com.github.leeonky.util.Classes;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;

public class ListInspector extends CacheableInspector {

    public ListInspector(Data data) {
        super(data);
    }

    @Override
    public String cachedInspect(String path, InspectorCache cache) {
        List<Data> dataList = data.getDataList();
        String type = type();
        if (dataList.isEmpty())
            return type + "[]";
        AtomicInteger index = new AtomicInteger(0);
        return type + dataList.stream().map(data ->
                        data.buildInspector().dump(path + "[" + index.getAndIncrement() + "]", cache))
                .map(TextUtil::indent).collect(joining(",\n", "[\n", "\n]"));
    }

    protected String type() {
        if (data.getInstance() instanceof Iterable || data.getInstance() instanceof Stream
                || data.getInstance().getClass().isArray())
            return "";
        return Classes.getClassName(data.getInstance()) + " ";
    }
}
