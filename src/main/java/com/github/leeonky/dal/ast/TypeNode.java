package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

import java.util.Objects;

public class TypeNode extends Node {
    private final String type;

    public TypeNode(String type) {
        this.type = type;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        return null;
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof TypeNode && Objects.equals(type, ((TypeNode) obj).type);
    }
}
