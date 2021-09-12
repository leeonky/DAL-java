package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.util.DataObject;
import com.github.leeonky.dal.util.ListAccessor;
import com.github.leeonky.dal.util.PropertyAccessor;
import com.github.leeonky.dal.util.TypeData;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.Converter;

import java.lang.reflect.Array;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class RuntimeContext {
    private final LinkedList<DataObject> thisStack = new LinkedList<>();
    private final TypeData<PropertyAccessor<Object>> propertyAccessors;
    private final TypeData<ListAccessor<Object>> listAccessors;
    private final Map<String, ConstructorViaSchema> constructors;
    private final Set<Class<?>> schemas;
    private final Map<String, BeanClass<?>> schemaMap;
    private final Converter converter = Converter.createDefault();

    @SuppressWarnings("unchecked")
    public RuntimeContext(Object inputValue, TypeData<PropertyAccessor<?>> propertyAccessors,
                          Map<String, ConstructorViaSchema> constructors, TypeData<ListAccessor<?>> listAccessors,
                          Map<String, BeanClass<?>> schemas) {
        this.schemas = schemas.values().stream().map(BeanClass::getType).collect(Collectors.toSet());
        schemaMap = schemas;
        thisStack.push(wrap(inputValue));
        this.constructors = constructors;
        this.propertyAccessors = (TypeData) propertyAccessors;
        this.listAccessors = (TypeData) listAccessors;
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

    public boolean isRegistered(Class<?> fieldType) {
        return schemas.contains(fieldType);
    }

    public Optional<Set<String>> findPropertyReaderNames(Object instance) {
        return propertyAccessors.getData(instance).map(f -> f.getPropertyNames(instance));
    }

    public Boolean isNull(Object instance) {
        return propertyAccessors.getData(instance).map(f -> f.isNull(instance))
                .orElseGet(() -> Objects.equals(instance, null));
    }

    public Optional<Object> getPropertyValue(Object instance, String name) {
        return propertyAccessors.getData(instance).map(f -> f.getValue(instance, name));
    }

    @SuppressWarnings("unchecked")
    public Iterable<Object> getList(Object instance) {
        return listAccessors.getData(instance).map(l -> (Iterable<Object>) l.toIterable(instance))
                .orElseGet(() -> () -> new Iterator<Object>() {
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
                });
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
}
