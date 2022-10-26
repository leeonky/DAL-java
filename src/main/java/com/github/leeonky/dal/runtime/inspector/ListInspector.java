package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.util.TextUtil;
import com.github.leeonky.util.Classes;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;

//TODO refactor
public class ListInspector implements Inspector.Cacheable {

    @Override
    public String cachedInspect(Data data, InspectorContext context) {
        List<Data> dataList = data.getDataList();
        String type = type(data);
        if (dataList.isEmpty())
            return type + "[]";
        AtomicInteger index = new AtomicInteger(0);
        return type + dataList.stream().map(subData -> context.index(index.getAndIncrement()).dump(subData))
                .map(TextUtil::indent).collect(joining(",\n", "[\n", "\n]"));
    }

    protected String type(Data data) {
        if (data.getInstance() instanceof Iterable || data.getInstance() instanceof Stream
                || data.getInstance().getClass().isArray())
            return "";
        return Classes.getClassName(data.getInstance()) + " ";
    }
}
