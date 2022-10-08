package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.util.TextUtil;

import java.util.List;

import static com.github.leeonky.util.Classes.getClassName;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class Inspector {
    protected final Data data;

    public Inspector(Data data) {
        this.data = data;
    }

    public String inspectType() {
        return data.isNull() ? "null" : getClassName(data.getInstance());
    }

    public String inspectValue() {
        if (data.isNull())
            return "";
        else if (data.isList()) {
            List<Data> dataList = data.getDataList();
            if (dataList.isEmpty())
                return "[]";
            return dataList.stream().map(Data::inspect).map(TextUtil::indent).collect(joining(", ", "[\n", "\n]"));
        }
        return format("<%s>", data.getInstance().toString()
                .replace("\\\\", "\\").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t"));
    }

    public String inspect() {
        return String.join("\n", inspectType(), inspectValue()).trim();
    }
}
