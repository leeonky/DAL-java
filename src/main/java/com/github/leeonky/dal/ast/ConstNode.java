package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Objects;

public class ConstNode extends DALNode {

    private final Object value;

    public ConstNode(Object value) {
        this.value = value;
    }

    public Object getValue() {
        return value;
    }

    @Override
    public Object evaluate(RuntimeContextBuilder.RuntimeContext context) {
        return value;
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof ConstNode && Objects.equals(value, ((ConstNode) obj).value);
    }

    @Override
    public String inspect() {
        if (value == null)
            return "null";
        if (value instanceof String)
            return String.format("'%s'", value);
        return value.toString();
    }
}
