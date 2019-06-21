package com.github.leeonky.dal.util;

import java.lang.reflect.Array;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

public class WrappedObject {
    private final Object instance;
    private final TypeData<PropertyAccessor> propertyAccessors;
    private final TypeData<ListAccessor> listAccessors;

    public WrappedObject(Object instance, TypeData<PropertyAccessor> propertyAccessors, TypeData<ListAccessor> listAccessors) {
        this.instance = instance;
        this.propertyAccessors = propertyAccessors;
        this.listAccessors = listAccessors;
    }

    @SuppressWarnings("unchecked")
    public Set<String> getPropertyNames() {
        return propertyAccessors.getData(instance)
                .map(f -> f.getPropertyNames(instance))
                .orElseGet(() -> {
                    if (instance instanceof Map)
                        return ((Map) instance).keySet();
                    return BeanUtil.findPropertyNames(instance.getClass());
                });
    }

    public Object getPropertyValue(String name) {
        return instance instanceof Map ?
                ((Map) instance).get(name)
                : getPropertyFromType(name);
    }

    public Object getInstance() {
        return instance;
    }

    public WrappedObject getPropertyValueWrapper(String name) {
        return new WrappedObject(getPropertyValue(name), propertyAccessors, listAccessors);
    }

    @SuppressWarnings("unchecked")
    public int getListSize() {
        return listAccessors.getData(instance)
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

    @SuppressWarnings("unchecked")
    private Object getPropertyFromType(String name) {
        return propertyAccessors.getData(instance)
                .map(p -> checkedReturn(() -> p.getValue(instance, name)))
                .orElseGet(() -> checkedReturn(() -> BeanUtil.getPropertyValue(instance, name)));
    }

    private Object checkedReturn(CheckedSupplier supplier) {
        try {
            return supplier.get();
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    interface CheckedSupplier {
        Object get() throws Exception;
    }
}
