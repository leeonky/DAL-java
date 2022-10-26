package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

@Deprecated
public class ValueInspectorBk implements TypeValueInspectorBk {

    private final Data data;

    public ValueInspectorBk(Data data) {
        this.data = data;
    }

    @Override
    public String inspectType() {
        return Classes.getClassName(data.getInstance());
    }

    @Override
    public String inspectValue() {
        return "<" + data.getInstance().toString() + ">";
    }
}
