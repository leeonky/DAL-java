package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.RuntimeException;

import java.util.Objects;
import java.util.function.Function;

public class TypeNode extends Node {
    private final String type;

    public TypeNode(String type) {
        this.type = type;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        final Function<Object, Object> function = context.getTypes().get(type);
        if (function == null)
            throw new RuntimeException("Type '" + type + "' not registered", getPositionBegin());
        return function;
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof TypeNode && Objects.equals(type, ((TypeNode) obj).type);
    }
}
