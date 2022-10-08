package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.util.TextUtil;

import java.util.List;
import java.util.Set;

import static com.github.leeonky.util.Classes.getClassName;
import static java.util.stream.Collectors.joining;

public class DefaultInspector implements Inspector {
    protected final Data data;

    public DefaultInspector(Data data) {
        this.data = data;
    }

    @Override
    public String inspectType() {
        return data.isNull() ? "null" : getClassName(data.getInstance());
    }

    @Override
    public String inspectValue() {
        if (data.isNull())
            return "";
        else if (data.isList()) {
            List<Data> dataList = data.getDataList();
            if (dataList.isEmpty())
                return "[]";
            return dataList.stream().map(Data::dump).map(TextUtil::indent).collect(joining(",\n", "[\n", "\n]"));
        }
        Set<Object> fieldNames = data.getFieldNames();
        if (fieldNames.isEmpty())
            return "{}";
        return fieldNames.stream().map(o -> o + ": " + data.getValue(o).dump()).map(TextUtil::indent).collect(joining(",\n", "{\n", "\n}"));

//        return format("<%s>", data.getInstance().toString()
//                .replace("\\\\", "\\").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t"));
    }
}
