package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;

import java.util.Objects;

public class TypeNode extends Node {
    private final String type;

    public TypeNode(String type) {
        this.type = type;
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        return context.searchConstructor(type).orElseThrow(() ->
                new RuntimeException("Schema '" + type + "' not registered", getPositionBegin()));
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof TypeNode && Objects.equals(type, ((TypeNode) obj).type);
    }
}
