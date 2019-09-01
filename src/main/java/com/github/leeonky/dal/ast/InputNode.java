package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;

public class InputNode extends Node {
    public static final InputNode INSTANCE = new InputNode();

    private InputNode() {
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        return context.getInputValue();
    }

    @Override
    public String inspect() {
        return "";
    }
}
