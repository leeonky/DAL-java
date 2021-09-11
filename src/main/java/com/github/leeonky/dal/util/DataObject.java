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

public class DataObject {
    //TODO private
    public final SchemaType schemaType;
    private final RuntimeContext runtimeContext;
    private final Object instance;
    private final BeanClass<Object> beanClass;

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
    public Set<String> getFieldNames() {
        return runtimeContext.findPropertyReaderNames(instance)
                .orElseGet(() -> instance instanceof Map ? ((Map) instance).keySet()
                        : beanClass.getPropertyReaders().keySet());
    }

    public long getListSize() {
        return StreamSupport.stream(getListValues().spliterator(), false).count();
    }

    @SuppressWarnings("unchecked")
    private Iterable<Object> getListValues() {
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

    public Iterable<DataObject> asList() {
        List<DataObject> result = new ArrayList<>();
        int i = 0;
        for (Object object : getListValues())
            result.add(new DataObject(object, runtimeContext, schemaType.access(i++)));
        return result;
    }

    public boolean isNull() {
        return runtimeContext.isNull(instance);
    }

    public SchemaVerifier createSchemaVerifier() {
        return new SchemaVerifier(runtimeContext, this);
    }

    public DataObject getValue(Object... properties) {
        return getValue(new LinkedList<>(Arrays.asList(properties)));
    }

    private DataObject getValue(LinkedList<Object> properties) {
        if (properties.isEmpty())
            return this;
        return getValue(properties.removeFirst()).getValue(properties);
    }

    public DataObject getValue(Object property) {
        List<Object> propertyChain = schemaType.access(property).getPropertyChainBefore(schemaType);
        if (propertyChain.size() == 1 && propertyChain.get(0).equals(property))
            return new DataObject(getElementOrPropertyValue(property), runtimeContext, schemaType.access(property));
        return getValue(new LinkedList<>(propertyChain));
    }

    private Object getElementOrPropertyValue(Object property) {
        if (isList()) {
            if ("size".equals(property))
                return getListSize();
            //TODO process schema and data object
            if (property instanceof String)
                return StreamSupport.stream(asList().spliterator(), false)
                        .map(e -> e.getElementOrPropertyValue(property))
                        .collect(Collectors.toList());
            return getElement((int) property);
        }
        return getPropertyValue((String) property);
    }

    private Object getElement(int index) {
        return StreamSupport.stream(getListValues().spliterator(), false).skip(index).findFirst()
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

    public Object guessField(Object rootName) {
        return schemaType.access(rootName).getPropertyChainBefore(schemaType).get(0);
    }
}
