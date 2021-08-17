package com.github.leeonky.dal.util;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.util.BeanClass;

import java.lang.reflect.Array;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static java.util.Arrays.asList;

public class DataObject {
    private final RuntimeContext runtimeContext;
    private final Object instance;
    private final BeanClass<Object> beanClass;

    @SuppressWarnings("unchecked")
    public DataObject(Object instance, RuntimeContext context) {
        this.instance = instance;
        beanClass = instance == null ? null : (BeanClass<Object>) BeanClass.create(instance.getClass());
        runtimeContext = context;
    }

    public Object getInstance() {
        return instance;
    }

    public boolean isList() {
        return instance != null && (runtimeContext.isRegisteredList(instance) || instance instanceof Iterable || instance.getClass().isArray());
    }

    @SuppressWarnings("unchecked")
    public Set<String> getPropertyReaderNames() {
        return runtimeContext.findPropertyReaderNames(instance)
                .orElseGet(() -> {
                    if (instance instanceof Map)
                        return ((Map) instance).keySet();
                    return beanClass.getPropertyReaders().keySet();
                });
    }

    public DataObject getWrappedPropertyValue(String name) {
        return runtimeContext.wrap(getValue(name));
    }

    public int getListSize() {
        int size = 0;
        for (Object ignore : getList())
            size++;
        return size;
    }

    @SuppressWarnings("unchecked")
    public Iterable<Object> getList() {
        return runtimeContext.gitList(instance)
                .orElseGet(() -> {
                    if (instance instanceof Iterable)
                        return (Iterable<Object>) instance;
                    return () -> new Iterator<Object>() {
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

    public Iterable<DataObject> getWrappedList() {
        List<DataObject> result = new ArrayList<>();
        for (Object object : getList())
            result.add(runtimeContext.wrap(object));
        return result;
    }

    private Object getPropertyValue(String name) {
        if (instance instanceof Map)
            return ((Map<?, ?>) instance).get(name);
        return runtimeContext.getPropertyValue(instance, name)
                .orElseGet(() -> beanClass.getPropertyValue(instance, name));
    }

    public boolean isNull() {
        return runtimeContext.isNull(instance);
    }

    public SchemaVerifier createSchemaVerifier() {
        return new SchemaVerifier(runtimeContext, this);
    }

    public Object getValue(Object... properties) {
        return getValue(new LinkedList<>(asList(properties)));
    }

    private Object getValue(LinkedList<Object> properties) {
        Object object = getValue(properties.removeFirst());
        return properties.isEmpty() ? object : runtimeContext.wrap(object).getValue(properties);
    }

    private Object getValue(Object property) {
        if (isList()) {
            if ("size".equals(property))
                return getListSize();
            if (property instanceof String) {
                return StreamSupport.stream(getList().spliterator(), false)
                        .map(runtimeContext::wrap)
                        .map(e -> e.getValue(property))
                        .collect(Collectors.toList());
            }
            return getElement((int) property);
        }
        return getPropertyValue((String) property);
    }

    private Object getElement(int index) {
        return StreamSupport.stream(getList().spliterator(), false).skip(index).findFirst()
                .orElseThrow(() -> new IndexOutOfBoundsException("Index out of range: " + index));
    }
}
