package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CheckedBiFunction;
import com.github.leeonky.dal.CompilingContext;

import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;

public class PropertyNode implements Node {
    private final Node instanceNode;
    private final List<String> properties;

    public PropertyNode(Node instanceNode, List<String> properties) {
        this.instanceNode = instanceNode;
        this.properties = properties;
    }

    private String capitalize(String str) {
        return str.toUpperCase().substring(0, 1) + str.substring(1);
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
                : getPropertyThroughCustomerType(instance, name, context, this::getPropertyThroughBean);
    }

    @SuppressWarnings("unchecked")
    private Object getPropertyThroughCustomerType(Object instance, String name, CompilingContext context, BiFunction<Object, String, Object> defaultGetter) {
        return context.getRegisterTypes().entrySet().stream()
                .filter(e -> e.getKey().isInstance(instance))
                .map(e -> {
                    try {
                        return ((CheckedBiFunction) e.getValue()).apply(instance, name);
                    } catch (Exception ex) {
                        throw new IllegalStateException(ex);
                    }
                }).findFirst().orElseGet(() -> defaultGetter.apply(instance, name));
    }

    private Object getPropertyThroughBean(Object instance, String name) {
        try {
            return instance.getClass().getField(name).get(instance);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            try {
                return instance.getClass().getMethod("get" + capitalize(name)).invoke(instance);
            } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException ex) {
                throw new IllegalStateException();
            }
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof PropertyNode
                && Objects.equals(instanceNode, ((PropertyNode) obj).instanceNode)
                && Objects.equals(properties, ((PropertyNode) obj).properties);
    }
}
