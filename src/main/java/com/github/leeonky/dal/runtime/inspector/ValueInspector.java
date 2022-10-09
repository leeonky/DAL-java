package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.Classes;

public class ValueInspector implements TypeValueInspector {

    private final Data data;

    public ValueInspector(Data data) {
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
