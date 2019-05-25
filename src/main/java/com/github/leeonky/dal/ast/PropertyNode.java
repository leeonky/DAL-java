package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

public class PropertyNode implements Node {
    private final String property;

    public PropertyNode(String property) {
        this.property = property;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        throw new IllegalStateException();
    }
}
