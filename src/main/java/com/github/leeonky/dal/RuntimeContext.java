package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.util.ListAccessor;
import com.github.leeonky.dal.util.PropertyAccessor;
import com.github.leeonky.dal.util.TypeData;
import com.github.leeonky.dal.util.WrappedObject;

import java.util.*;

public class RuntimeContext {
    private final TypeData<PropertyAccessor> propertyAccessors;
    private final TypeData<ListAccessor> listAccessors;
    private final LinkedList<Object> wrappedValueStack = new LinkedList<>();
    private final Map<String, Constructor> constructors;
    private final Set<Class<?>> schemas;

    public RuntimeContext(Object inputValue, TypeData<PropertyAccessor> propertyAccessors,
                          Map<String, Constructor> constructors, TypeData<ListAccessor> listAccessors, Set<Class<?>> schemas) {
        this.schemas = schemas;
        wrappedValueStack.push(inputValue);
        this.constructors = constructors;
        this.propertyAccessors = propertyAccessors;
        this.listAccessors = listAccessors;
    }

    public Object getInputValue() {
        return wrappedValueStack.getFirst();
    }

    public Object wrapInputValueAndEvaluate(Object value, Node node) {
        try {
            wrappedValueStack.push(value);
            return node.evaluate(this);
        } finally {
            wrappedValueStack.pop();
        }
    }

    public Optional<Constructor> searchConstructor(String type) {
        return Optional.ofNullable(constructors.get(type));
    }

    public Optional<ListAccessor> searchListAccessor(Object object) {
        return listAccessors.getData(object);
    }

    public WrappedObject wrap(Object instance) {
        return new WrappedObject(instance, this);
    }

    public boolean isRegistered(Class<?> fieldType) {
        return schemas.contains(fieldType);
    }

    @SuppressWarnings("unchecked")
    public Optional<Set> findPropertyReaderNames(Object instance) {
        return propertyAccessors.getData(instance)
                .map(f -> f.getPropertyNames(instance));
    }

    @SuppressWarnings("unchecked")
    public Boolean isNull(Object instance) {
        return propertyAccessors.getData(instance)
                .map(p -> p.isNull(instance))
                .orElseGet(() -> Objects.equals(instance, null));
    }

    @SuppressWarnings("unchecked")
    public Optional<Object> getPropertyValue(Object instance, String name) {
        return propertyAccessors.getData(instance)
                .map(p -> p.getValue(instance, name));
    }

    @SuppressWarnings("unchecked")
    public Optional<Iterable> gitList(Object instance) {
        return listAccessors.getData(instance)
                .map(l -> l.toIterable(instance));
    }

    public boolean isRegisteredList(Object instance) {
        return listAccessors.containsType(instance);
    }
}
