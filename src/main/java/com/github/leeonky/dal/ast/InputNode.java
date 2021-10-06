package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.DataObject;
import com.github.leeonky.dal.runtime.RuntimeContext;

public class InputNode extends Node {
    public static final InputNode INSTANCE = new InputNode();

    private InputNode() {
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        return evaluateDataObject(context).getInstance();
    }

    @Override
    public DataObject evaluateDataObject(RuntimeContext context) {
        return context.getInputValue();
    }

    @Override
    public String inspect() {
        return "";
    }
}
