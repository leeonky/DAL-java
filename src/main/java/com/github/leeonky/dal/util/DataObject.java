package com.github.leeonky.dal.util;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.SchemaType;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.NoSuchAccessorException;

import java.lang.reflect.Array;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static java.util.Arrays.asList;

//TODO clean methods *****
public class DataObject {
    private final RuntimeContext runtimeContext;
    private final Object instance;
    private final BeanClass<Object> beanClass;
    private final SchemaType schemaType;

    @SuppressWarnings("unchecked")
    public DataObject(Object instance, RuntimeContext context, SchemaType schemaType) {
        this.instance = instance;
        beanClass = instance == null ? null : (BeanClass<Object>) BeanClass.create(instance.getClass());
        runtimeContext = context;
        this.schemaType = schemaType;
    }

    public Object getInstance() {
        return instance;
    }

    public boolean isList() {
        return instance != null && (runtimeContext.isRegisteredList(instance) || instance instanceof Iterable
                || instance.getClass().isArray() || instance instanceof Stream);
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

    public int getListSize() {
        int size = 0;
        for (Object ignore : getList())
            size++;
        return size;
    }

    @SuppressWarnings("unchecked")
    //TODO return data object ****
    public Iterable<Object> getList() {
        return runtimeContext.gitList(instance)
                .orElseGet(() -> {
                    if (instance instanceof Iterable)
                        return (Iterable<Object>) instance;
                    if (instance instanceof Stream)
                        return ((Stream<Object>) instance)::iterator;
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

    //TODO process schema ***********
    public Iterable<DataObject> getWrappedList() {
        List<DataObject> result = new ArrayList<>();
        for (Object object : getList())
            result.add(runtimeContext.wrap(object));
        return result;
    }

    public boolean isNull() {
        return runtimeContext.isNull(instance);
    }

    public SchemaVerifier createSchemaVerifier() {
        return new SchemaVerifier(runtimeContext, this);
    }

    public DataObject getValue(Object... properties) {
        return getValue(new LinkedList<>(asList(properties)));
    }

    private DataObject getValue(LinkedList<Object> properties) {
        if (properties.isEmpty())
            return this;
        return getValue(properties.removeFirst()).getValue(properties);
    }

    public DataObject getValue(Object property) {
        List<Object> propertyChainBefore = schemaType.access(property).getPropertyChainBefore(schemaType);
        if (propertyChainBefore.size() == 1 && propertyChainBefore.get(0).equals(property))
            return new DataObject(getElementOrPropertyValue(propertyChainBefore.get(0)), runtimeContext, schemaType.access(property));
        return getValue(new LinkedList<>(propertyChainBefore));
    }

    private Object getElementOrPropertyValue(Object property) {
        if (isList()) {
            if ("size".equals(property))
                return getListSize();
            if (property instanceof String) {
                //TODO process schema and data object
                return StreamSupport.stream(getList().spliterator(), false)
                        .map(runtimeContext::wrap)
                        .map(e -> e.getElementOrPropertyValue(property))
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

    private Object getPropertyValue(String name) {
        if (instance instanceof Map)
            return ((Map<?, ?>) instance).get(name);
        return runtimeContext.getPropertyValue(instance, name)
                .orElseGet(() -> getBeanPropertyValue(name));
    }

    private Object getBeanPropertyValue(String name) {
        try {
            return beanClass.getPropertyValue(instance, name);
        } catch (NoSuchAccessorException ignore) {
            try {
                return beanClass.getType().getMethod(name).invoke(instance);
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
        }
    }
}
