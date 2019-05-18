package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

public class ConstNode implements Node {
    private final Object value;

    public ConstNode(Object value) {
        this.value = value;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        return value;
    }
}
