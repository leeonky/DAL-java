package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.util.BeanUtil;

import java.lang.reflect.Array;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
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
        return instance instanceof Map ?
                ((Map) instance).get(name)
                : getPropertyFromType(instance, name, context);
    }

    @SuppressWarnings("unchecked")
    private Object getPropertyFromType(Object instance, String name, CompilingContext context) {
        try {
            if ("size".equals(name) && context.isList(instance))
                return getListSize(instance, context);
            return context.searchPropertyAccessor(instance)
                    .map(p -> checkedReturn(() -> p.getValue(instance, name)))
                    .orElseGet(() -> checkedReturn(() -> BeanUtil.getPropertyValue(instance, name)));
        } catch (Exception e) {
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

    private <T> T checkedReturn(CheckedSupplier<T> supplier) {
        try {
            return supplier.get();
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof PropertyNode
                && Objects.equals(instanceNode, ((PropertyNode) obj).instanceNode)
                && Objects.equals(properties, ((PropertyNode) obj).properties);
    }

    interface CheckedSupplier<T> {
        T get() throws Exception;
    }
}
