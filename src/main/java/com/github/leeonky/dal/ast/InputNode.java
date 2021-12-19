package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class InputNode extends Node {
    public static final InputNode INSTANCE = new InputNode();

    private InputNode() {
    }

    @Override
    public Object evaluate(RuntimeContextBuilder.RuntimeContext context) {
        return evaluateDataObject(context).getInstance();
    }

    @Override
    public Data evaluateDataObject(RuntimeContextBuilder.RuntimeContext context) {
        return context.getInputValue();
    }

    @Override
    public String inspect() {
        return "";
    }
}
