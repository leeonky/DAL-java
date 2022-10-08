package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.util.TextUtil;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.github.leeonky.util.Classes.getClassName;
import static java.util.stream.Collectors.joining;

public interface Inspector {
    //    TODO refactor
    static Inspector defaultInspector(Data data) {
        if (data.isNull())
            return () -> "null";
        if (data.isList()) {
            return () -> {
                List<Data> dataList = data.getDataList();
                if (dataList.isEmpty())
                    return "[]";
                return dataList.stream().map(Data::dump).map(TextUtil::indent).collect(joining(",\n", "[\n", "\n]"));
            };
        }
        return () -> {
            String type = data.getInstance() instanceof Map ? "" : getClassName(data.getInstance()) + " ";
            Set<Object> fieldNames = data.getFieldNames();
            if (fieldNames.isEmpty())
                return type + "{}";
            return type + fieldNames.stream().map(o -> {
                        Data value;
                        try {
                            value = data.getValue(o);
                        } catch (Exception e) {
                            return o + ": *throw* " + e;
                        }
                        return o + ": " + value.dump();
                    }).map(TextUtil::indent)
                    .collect(joining(",\n", "{\n", "\n}"));
        };
    }

    String inspect();

    default String dump() {
        return inspect();
    }
}
