package com.github.leeonky.dal.runtime;

import static com.github.leeonky.util.Classes.getClassName;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class Inspector {
    private final Data data;

    public Inspector(Data data) {
        this.data = data;
    }

    public Data getData() {
        return data;
    }

    public String inspectType() {
        return getData().isNull() ? "null" : getClassName(getData().getInstance());
    }

    public String inspectValue() {
        if (getData().isNull())
            return "";
        else if (getData().isList())
            return getData().getDataList().stream().map(Data::inspectBk).collect(joining(", ", "[", "]"));
        return format("<%s>", getData().getInstance().toString()
                .replace("\\\\", "\\").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t"));
    }

    public String inspect() {
        return String.join("\n", inspectType(), inspectValue()).trim();
    }
}
