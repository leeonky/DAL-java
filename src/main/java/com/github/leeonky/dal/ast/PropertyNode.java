package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;

import java.util.List;
import java.util.Objects;

public class PropertyNode extends Node {
    private final Node instanceNode;
    private final List<String> properties;
    private final String propertyChain;

    public PropertyNode(Node instanceNode, List<String> properties) {
        this.instanceNode = instanceNode;
        this.properties = properties;
        propertyChain = String.join(".", properties);
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        try {
            return context.wrap(instanceNode.evaluate(context)).getPropertyValue(propertyChain);
        } catch (Exception e) {
            throw new RuntimeException("Get property '" + propertyChain + "' failed, property can be public field, getter or customer type getter",
                    getPositionBegin());
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof PropertyNode
                && Objects.equals(instanceNode, ((PropertyNode) obj).instanceNode)
                && Objects.equals(properties, ((PropertyNode) obj).properties);
    }

    @Override
    public String inspect() {
        return String.format("%s.%s", instanceNode.inspect(), String.join(".", properties));
    }
}
