package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.SyntaxException;

import java.util.Objects;

public class ConstNode extends Node {

    private final Object value;

    public ConstNode(Object value) {
        this.value = value;
    }

    public Object toPropertyOrListIndex() {
        if (value instanceof Number)
            if (value instanceof Long || value instanceof Integer)
                return value;
            else
                throw new SyntaxException("index must be integer", getPositionBegin());
        return value;
    }

    public static String inspectValue(Object value) {
        if (value == null)
            return "null";
        if (value instanceof String)
            return String.format("'%s'", value);
        return value.toString();
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        return value;
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof ConstNode && Objects.equals(value, ((ConstNode) obj).value);
    }

    @Override
    public String inspect() {
        return inspectValue(value);
    }
}
