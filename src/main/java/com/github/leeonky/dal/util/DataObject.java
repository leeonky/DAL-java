package com.github.leeonky.dal.util;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.SchemaType;

import java.util.*;
import java.util.stream.StreamSupport;

import static java.util.stream.Collectors.toList;
import static java.util.stream.StreamSupport.stream;

public class DataObject {
    private final SchemaType schemaType;
    private final RuntimeContext runtimeContext;
    private final Object instance;
    private List<Object> listValue;

    public DataObject(Object instance, RuntimeContext context, SchemaType schemaType) {
        this.instance = instance;
        this.schemaType = schemaType;
        runtimeContext = context.registerPropertyAccessor(instance);
    }

    public Object getInstance() {
        return instance;
    }

    public Set<String> getFieldNames() {
        return runtimeContext.findPropertyReaderNames(instance);
    }

    public boolean isList() {
        return runtimeContext.isRegisteredList(instance) || (instance != null && instance.getClass().isArray());
    }

    public long getListSize() {
        return getListValues().size();
    }

    private List<Object> getListValues() {
        if (listValue == null)
            listValue = stream(runtimeContext.getList(instance).spliterator(), false).collect(toList());
        return listValue;
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
                        .collect(toList());
            return getListValues().get((int) property);
        }
        return runtimeContext.getPropertyValue(instance, (String) property);
    }

    public Object filedNameFromAlias(Object rootName) {
        return schemaType.access(rootName).getPropertyChainBefore(schemaType).get(0);
    }
}
