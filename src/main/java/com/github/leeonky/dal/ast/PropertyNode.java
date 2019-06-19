package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.BeanWrapper;
import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.RuntimeException;

import java.lang.reflect.Array;
import java.util.Iterator;
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

    @SuppressWarnings("unchecked")
    private Object getPropertyValue(Object instance, String name, CompilingContext context) {
        if ("size".equals(name) && context.isList(instance))
            return getListSize(instance, context);
        try {
            return new BeanWrapper(instance, context.getPropertyAccessors()).getPropertyValue(name);
        } catch (IllegalStateException e) {
            throw new RuntimeException("Get property " + name + " failed, property can be public field, getter or customer type getter",
                    getPositionBegin());
        }
    }

    private Object getListSize(Object instance, CompilingContext context) {
        return context.searchListAccessor(instance)
                .map(p -> p.size(instance))
                .orElseGet(() -> {
                    if (instance instanceof Iterable) {
                        Iterator iterator = ((Iterable) instance).iterator();
                        int size = 0;
                        while (iterator.hasNext()) {
                            iterator.next();
                            size++;
                        }
                        return size;
                    }
                    return Array.getLength(instance);
                });
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof PropertyNode
                && Objects.equals(instanceNode, ((PropertyNode) obj).instanceNode)
                && Objects.equals(properties, ((PropertyNode) obj).properties);
    }
}
