package com.github.leeonky.dal.util;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.type.SubType;
import com.github.leeonky.util.BeanClass;

import java.lang.reflect.Array;
import java.util.*;
import java.util.stream.Stream;

public class WrappedObject {
    private final Object instance;
    private final TypeData<PropertyAccessor> propertyAccessors;
    private final TypeData<ListAccessor> listAccessors;
    private final BeanClass beanClass;

    public WrappedObject(Object instance, TypeData<PropertyAccessor> propertyAccessors, TypeData<ListAccessor> listAccessors) {
        this.instance = instance;
        this.propertyAccessors = propertyAccessors;
        this.listAccessors = listAccessors;
        beanClass = instance == null ? null : BeanClass.create(instance.getClass());
    }

    public boolean isList(RuntimeContext context) {
        return instance != null && (listAccessors.containsType(instance) || instance instanceof Iterable || instance.getClass().isArray());
    }

    @SuppressWarnings("unchecked")
    public Set<String> getPropertyReaderNames() {
        return propertyAccessors.getData(instance)
                .map(f -> f.getPropertyNames(instance))
                .orElseGet(() -> {
                    if (instance instanceof Map)
                        return ((Map) instance).keySet();
                    return beanClass.getPropertyReaders().keySet();
                });
    }

    public Object getPropertyValue(String name) {
        if (name.contains(".")) {
            String[] split = name.split("\\.", 2);
            return getPropertyValueWrapper(split[0]).getPropertyValue(split[1]);
        }
        return instance instanceof Map ?
                ((Map) instance).get(name)
                : getPropertyFromType(name);
    }

    public Object getValue() {
        return instance;
    }

    public WrappedObject getPropertyValueWrapper(String name) {
        return new WrappedObject(getPropertyValue(name), propertyAccessors, listAccessors);
    }

    public int getListSize() {
        int size = 0;
        for (Object ignore : getList())
            size++;
        return size;
    }

    @SuppressWarnings("unchecked")
    public Iterable getList() {
        return listAccessors.getData(instance)
                .map(l -> l.toIterable(instance))
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
            result.add(new WrappedObject(object, propertyAccessors, listAccessors));
        return result;
    }

    @SuppressWarnings("unchecked")
    private Object getPropertyFromType(String name) {
        return propertyAccessors.getData(instance)
                .map(p -> checkedReturn(() -> p.getValue(instance, name)))
                .orElseGet(() -> checkedReturn(() -> beanClass.getPropertyValue(instance, name)));
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

    public BeanClass getPolymorphicSchemaType(Class<?> superSchemaType) {
        Class<?> type = superSchemaType;
        SubType subType = superSchemaType.getAnnotation(SubType.class);
        if (subType != null) {
            Object value = getPropertyValue(subType.property());
            type = Stream.of(subType.types())
                    .filter(t -> t.value().equals(value))
                    .map(SubType.Type::type)
                    .findFirst().orElseThrow(() -> new IllegalStateException(String.format("Cannot guess sub type through property type value[%s]", value)));
        }
        return BeanClass.create(type);
    }

    interface CheckedSupplier {
        Object get() throws Exception;
    }
}
