package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.util.DataObject;

import java.util.Set;

public class ObjectNode extends Node {

    @Override
    public String inspect() {
        return "{}";
    }

    @Override
    public boolean judge(Operator.Equal operator, Object input, RuntimeContext context) {
        DataObject data = context.wrap(input);
        Set<String> dataFields = data.getPropertyReaderNames();
        if (dataFields.isEmpty())
            return true;
        System.err.printf("Warning: unexpected fields: %s\n", dataFields);
        return false;
    }

    @Override
    public boolean judge(Operator.Matcher operator, Object input, RuntimeContext context) {
        return input != null;
    }
}
