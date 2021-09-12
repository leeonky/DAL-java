package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.util.ClassKeyMap;
import com.github.leeonky.dal.util.DataObject;
import com.github.leeonky.dal.util.ListAccessor;
import com.github.leeonky.dal.util.PropertyAccessor;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.Converter;
import com.github.leeonky.util.NoSuchAccessorException;

import java.lang.reflect.Array;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class RuntimeContext {
    private final LinkedList<DataObject> thisStack = new LinkedList<>();
    private final ClassKeyMap<PropertyAccessor<Object>> propertyAccessors;
    private final ClassKeyMap<ListAccessor<Object>> listAccessors;
    private final Map<String, ConstructorViaSchema> constructors;
    private final Set<Class<?>> schemas;
    private final Map<String, BeanClass<?>> schemaMap;
    private final Converter converter = Converter.createDefault();

    @SuppressWarnings("unchecked")
    public RuntimeContext(Object inputValue, ClassKeyMap<PropertyAccessor<?>> propertyAccessors,
                          Map<String, ConstructorViaSchema> constructors, ClassKeyMap<ListAccessor<?>> listAccessors,
                          Map<String, BeanClass<?>> schemas) {
        this.schemas = schemas.values().stream().map(BeanClass::getType).collect(Collectors.toSet());
        schemaMap = schemas;
        this.constructors = constructors;
        this.propertyAccessors = (ClassKeyMap) propertyAccessors;
        this.listAccessors = (ClassKeyMap) listAccessors;
        thisStack.push(wrap(inputValue));
    }

    public DataObject getInputValue() {
        return thisStack.getFirst();
    }

    public Object wrapInputValueAndEvaluate(Object value, Node node, String schema) {
        return newThisScope(new DataObject(value, this, SchemaType.create(schemaMap.get(schema))),
                () -> node.evaluate(this));
    }

    public <T> T newThisScope(DataObject dataObject, Supplier<T> supplier) {
        try {
            thisStack.push(dataObject);
            return supplier.get();
        } finally {
            thisStack.pop();
        }
    }

    public Optional<ConstructorViaSchema> searchConstructor(String type) {
        return Optional.ofNullable(constructors.get(type));
    }

    public boolean isSchemaRegistered(Class<?> fieldType) {
        return schemas.contains(fieldType);
    }

    public Set<String> findPropertyReaderNames(Object instance) {
        return propertyAccessors.getData(instance).getPropertyNames(instance);
    }

    public Boolean isNull(Object instance) {
        return propertyAccessors.tryGetData(instance).map(f -> f.isNull(instance))
                .orElseGet(() -> Objects.equals(instance, null));
    }

    public Object getPropertyValue(Object instance, String name) {
        return propertyAccessors.getData(instance).getValue(instance, name);
    }

    @SuppressWarnings("unchecked")
    public Iterable<Object> getList(Object instance) {
        return listAccessors.tryGetData(instance).map(l -> (Iterable<Object>) l.toIterable(instance))
                .orElseGet(() -> arrayIterable(instance));
    }

    private Iterable<Object> arrayIterable(Object instance) {
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
    }

    public boolean isRegisteredList(Object instance) {
        return listAccessors.containsType(instance);
    }

    public Converter getConverter() {
        return converter;
    }

    public DataObject wrap(Object instance) {
        return new DataObject(instance, this, SchemaType.createRoot());
    }

    public RuntimeContext registerPropertyAccessor(Object instance) {
        if (!Objects.equals(instance, null) && !propertyAccessors.containsType(instance)) {
            propertyAccessors.put(BeanClass.getClass(instance), new PropertyAccessor<Object>() {
                private final BeanClass<Object> beanClass = BeanClass.createFrom(instance);

                @Override
                public Object getValue(Object instance1, String name) {
                    try {
                        return beanClass.getPropertyValue(instance1, name);
                    } catch (NoSuchAccessorException ignore) {
                        try {
                            return beanClass.getType().getMethod(name).invoke(instance1);
                        } catch (Exception e) {
                            throw new IllegalStateException(e);
                        }
                    }
                }

                @Override
                public Set<String> getPropertyNames(Object instance1) {
                    return beanClass.getPropertyReaders().keySet();
                }

                @Override
                public boolean isNull(Object instance1) {
                    return Objects.equals(instance1, null);
                }
            });
        }
        return this;
    }
}
