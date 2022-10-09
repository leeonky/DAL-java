package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.util.TextUtil;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static java.util.stream.Collectors.joining;

public class ListInspector extends CacheableInspector {

    public ListInspector(Data data) {
        super(data);
    }

    @Override
    public String cachedInspect(String path, InspectorCache cache) {
        List<Data> dataList = data.getDataList();
        if (dataList.isEmpty())
            return "[]";
        AtomicInteger index = new AtomicInteger(0);
        return dataList.stream().map(data -> data.buildInspector().dump(path + "[" + index.getAndIncrement() + "]", cache))
                .map(TextUtil::indent).collect(joining(",\n", "[\n", "\n]"));
    }
}
