package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

public class InputNode implements Node {

    @Override
    public Object evaluate(CompilingContext context) {
        return context.getInputValue();
    }
}
