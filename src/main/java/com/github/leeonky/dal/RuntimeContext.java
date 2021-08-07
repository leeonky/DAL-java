package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.util.DataObject;
import com.github.leeonky.dal.util.ListAccessor;
import com.github.leeonky.dal.util.PropertyAccessor;
import com.github.leeonky.dal.util.TypeData;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.Converter;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class RuntimeContext {
    private final LinkedList<SchemaType> schemaTypesStack = new LinkedList<>();
    private final TypeData<PropertyAccessor> propertyAccessors;
    private final TypeData<ListAccessor> listAccessors;
    private final LinkedList<Object> wrappedValueStack = new LinkedList<>();
    private final Map<String, ConstructorViaSchema> constructors;
    private final Set<Class<?>> schemas;
    private final Map<String, BeanClass<?>> schemaMap;
    private final Converter converter = Converter.createDefault();
    private int schemaTypePushDepth = 0, schemaTypePopDepth = 0;

    public RuntimeContext(Object inputValue, TypeData<PropertyAccessor> propertyAccessors,
                          Map<String, ConstructorViaSchema> constructors, TypeData<ListAccessor> listAccessors,
                          Map<String, BeanClass<?>> schemas) {
        this.schemas = schemas.values().stream().map(BeanClass::getType).collect(Collectors.toSet());
        schemaMap = schemas;
        wrappedValueStack.push(inputValue);
        //TODO null object
        schemaTypesStack.push(new SchemaType(null));
        this.constructors = constructors;
        this.propertyAccessors = propertyAccessors;
        this.listAccessors = listAccessors;
    }

    public Object getInputValue() {
        return wrappedValueStack.getFirst();
    }

    public Object wrapInputValueAndEvaluate(Object value, Node node, String schema) {
        try {
            wrappedValueStack.push(value);
            //TODO null object
            schemaTypesStack.push(new SchemaType(schemaMap.get(schema)));
            return node.evaluate(this);
        } finally {
            wrappedValueStack.pop();
            //TODO need test
            schemaTypesStack.pop();
        }
    }

    public Optional<ConstructorViaSchema> searchConstructor(String type) {
        return Optional.ofNullable(constructors.get(type));
    }

    public Optional<ListAccessor> searchListAccessor(Object object) {
        return listAccessors.getData(object);
    }

    public DataObject wrap(Object instance) {
        return new DataObject(instance, this);
    }

    public boolean isRegistered(Class<?> fieldType) {
        return schemas.contains(fieldType);
    }

    @SuppressWarnings("unchecked")
    public Optional<Set<String>> findPropertyReaderNames(Object instance) {
        return propertyAccessors.getData(instance).map(f -> f.getPropertyNames(instance));
    }

    @SuppressWarnings("unchecked")
    public Boolean isNull(Object instance) {
        return propertyAccessors.getData(instance).map(p -> p.isNull(instance))
                .orElseGet(() -> Objects.equals(instance, null));
    }

    @SuppressWarnings("unchecked")
    public Optional<Object> getPropertyValue(Object instance, String name) {
        return propertyAccessors.getData(instance).map(p -> p.getValue(instance, name));
    }

    @SuppressWarnings("unchecked")
    public Optional<Iterable<Object>> gitList(Object instance) {
        return listAccessors.getData(instance).map(l -> l.toIterable(instance));
    }

    public boolean isRegisteredList(Object instance) {
        return listAccessors.containsType(instance);
    }

    public Converter getConverter() {
        return converter;
    }

    public Object getAliasValue(Supplier<Object> input, Object alias) {
        schemaTypePushDepth++;
        DataObject dataObject = wrap(input.get());
        SchemaType currentSchema = schemaTypesStack.getFirst();
        SchemaType subSchema = currentSchema.access(alias);
        schemaTypesStack.push(subSchema);
        try {
            return dataObject.getValue(subSchema.getPropertyChainBefore(currentSchema).toArray());
        } finally {
            schemaTypePopDepth++;
            if (schemaTypePushDepth == schemaTypePopDepth) {
                for (schemaTypePopDepth = 0; schemaTypePushDepth > 0; schemaTypePushDepth--)
                    schemaTypesStack.pop();
            }
        }
    }
}
