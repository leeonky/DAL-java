package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

public class StringInspectorBk implements TypeValueInspectorBk {
    private final Data data;

    public StringInspectorBk(Data data) {
        this.data = data;
    }

    @Override
    public String inspectType() {
        return Classes.getClassName(data.getInstance());
    }

    @Override
    public String inspectValue() {
        return "<" + data.getInstance().toString().replace("\\", "\\\\").replace("\n", "\\n")
                .replace("\r", "\\r").replace("\t", "\\t").replace("\b", "\\b") + ">";
    }
}
