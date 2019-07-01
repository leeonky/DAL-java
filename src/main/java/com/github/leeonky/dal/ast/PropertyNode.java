package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.util.WrappedObject;

import java.util.List;
import java.util.Objects;

public class PropertyNode extends Node {
    private final Node instanceNode;
    private final List<String> properties;

    public PropertyNode(Node instanceNode, List<String> properties) {
        this.instanceNode = instanceNode;
        this.properties = properties;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        Object instance = instanceNode.evaluate(context);
        for (String name : properties)
            instance = getPropertyValue(instance, name, context);
        return instance;
    }

    private Object getPropertyValue(Object instance, String name, CompilingContext context) {
        WrappedObject wrappedObject = context.wrap(instance);
        if ("size".equals(name) && context.isList(instance))
            return wrappedObject.getListSize();
        try {
            return wrappedObject.getPropertyValue(name);
        } catch (IllegalStateException e) {
            throw new RuntimeException("Get property '" + name + "' failed, property can be public field, getter or customer type getter",
                    getPositionBegin());
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof PropertyNode
                && Objects.equals(instanceNode, ((PropertyNode) obj).instanceNode)
                && Objects.equals(properties, ((PropertyNode) obj).properties);
    }
}
