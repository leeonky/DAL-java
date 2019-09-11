package com.github.leeonky.dal.util;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.util.BeanClass;

import java.lang.reflect.Array;
import java.util.*;

public class WrappedObject {
    private final RuntimeContext runtimeContext;
    private final Object instance;
    private final BeanClass<Object> beanClass;

    @SuppressWarnings("unchecked")
    public WrappedObject(Object instance, RuntimeContext context) {
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

    public Object getPropertyValue(String name) {
        if (name.contains(".")) {
            String[] split = name.split("\\.", 2);
            return getWrappedPropertyValue(split[0]).getPropertyValue(split[1]);
        }
        return instance instanceof Map ?
                ((Map) instance).get(name)
                : getPropertyFromType(name);
    }

    public WrappedObject getWrappedPropertyValue(String name) {
        return runtimeContext.wrap(getPropertyValue(name));
    }

    public int getListSize() {
        int size = 0;
        for (Object ignore : getList())
            size++;
        return size;
    }

    public Iterable getList() {
        return runtimeContext.gitList(instance)
                .orElseGet(() -> {
                    if (instance instanceof Iterable)
                        return (Iterable) instance;
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
            result.add(runtimeContext.wrap(object));
        return result;
    }

    private Object getPropertyFromType(String name) {
        return runtimeContext.getPropertyValue(instance, name)
                .orElseGet(() -> beanClass.getPropertyValue(instance, name));
    }

    public boolean isNull() {
        return runtimeContext.isNull(instance);
    }

    public SchemaVerifier createSchemaVerifier() {
        return new SchemaVerifier(runtimeContext, this);
    }
}
