package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;

import java.util.Objects;

import static java.lang.String.format;

public class PropertyNode extends Node {
    private final Node instanceNode;
    private final Object name;

    public PropertyNode(Node instanceNode, Object property) {
        this.instanceNode = instanceNode;
        name = property;
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        try {
            return context.getAliasValue(() -> instanceNode.evaluate(context), name);
        } catch (Exception e) {
            //TODO log original and alias
            throw new RuntimeException(
                    format("Get property via `%s` failed, property can be public field, getter or customer type getter",
                            inspect()), getPositionBegin());
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof PropertyNode
                && Objects.equals(instanceNode, ((PropertyNode) obj).instanceNode)
                && Objects.equals(name, ((PropertyNode) obj).name);
    }

    @Override
    public String inspect() {
        return format("%s.%s", instanceNode.inspect(), name);
    }
}
