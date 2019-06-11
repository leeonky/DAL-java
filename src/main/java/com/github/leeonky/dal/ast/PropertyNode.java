package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CheckedBiFunction;
import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.RuntimeException;

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

    private static String capitalize(String str) {
        return str.toUpperCase().substring(0, 1) + str.substring(1);
    }

    private static Object getPropertyThroughBean(Object instance, String name) throws Exception {
        try {
            return instance.getClass().getMethod("get" + capitalize(name)).invoke(instance);
        } catch (Exception ex) {
            try {
                return instance.getClass().getMethod("is" + capitalize(name)).invoke(instance);
            } catch (Exception e) {
                return instance.getClass().getField(name).get(instance);
            }
        }
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
        CheckedBiFunction<?, String, Object> function = context.customerGetter(instance).orElse(PropertyNode::getPropertyThroughBean);
        try {
            return ((CheckedBiFunction) function).apply(instance, name);
        } catch (Exception e) {
            throw new RuntimeException("Get property " + name + " failed, property can be public field, getter or customer type getter",
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
