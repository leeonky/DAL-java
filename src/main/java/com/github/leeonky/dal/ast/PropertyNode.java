package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

import java.util.List;
import java.util.Objects;

public class PropertyNode implements Node {
    private final Node object;
    private final List<String> properties;

    public PropertyNode(Node object, List<String> properties) {
        this.object = object;
        this.properties = properties;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        throw new IllegalStateException();
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof PropertyNode
                && Objects.equals(object, ((PropertyNode) obj).object)
                && Objects.equals(properties, ((PropertyNode) obj).properties);
    }
}
