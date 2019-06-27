package com.github.leeonky.dal.util;

import java.lang.reflect.Array;
import java.util.*;

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
    public Set<String> getPropertyReaderNames() {
        return propertyAccessors.getData(instance)
                .map(f -> f.getPropertyNames(instance))
                .orElseGet(() -> {
                    if (instance instanceof Map)
                        return ((Map) instance).keySet();
                    return BeanUtil.findPropertyReaderNames(instance.getClass());
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

    public int getListSize() {
        int size = 0;
        for (Object dummy : getList())
            size++;
        return size;
    }

    @SuppressWarnings("unchecked")
    public Iterable getList() {
        return listAccessors.getData(instance)
                .map(l -> l.toIterable(instance))
                .orElseGet(() -> {
                    if (instance instanceof Iterable) {
                        return (Iterable) instance;
                    }
                    return () -> new Iterator() {
                        private final int length = Array.getLength(instance);
                        private int index = 0;

                        @Override
                        public boolean hasNext() {
                            return index < length;
                        }

                        @Override
                        public Object next() {
                            return Array.get(instance, index++);
                        }
                    };
                });
    }

    public Iterable<WrappedObject> getWrappedList() {
        List<WrappedObject> result = new ArrayList<>();
        for (Object object : getList())
            result.add(new WrappedObject(object, propertyAccessors, listAccessors));
        return result;
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

    @SuppressWarnings("unchecked")
    public boolean isNull() {
        return propertyAccessors.getData(instance)
                .map(p -> p.isNull(instance))
                .orElseGet(() -> Objects.equals(instance, null));
    }

    interface CheckedSupplier {
        Object get() throws Exception;
    }
}
