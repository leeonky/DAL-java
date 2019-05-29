package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

import java.util.Objects;

public class ConstNode extends Node {
    public static final ConstNode NULL_CONST_NODE = new ConstNode(null);

    private final Object value;

    public ConstNode(Object value) {
        this.value = value;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        return value;
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof ConstNode && Objects.equals(value, ((ConstNode) obj).value);
    }
}
