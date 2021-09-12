package com.github.leeonky.dal.util;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.SchemaType;

import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

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

    public List<DataObject> getListObjects() {
        AtomicInteger index = new AtomicInteger(0);
        return getListValues().stream().map(object ->
                new DataObject(object, runtimeContext, schemaType.access(index.incrementAndGet()))).collect(toList());
    }

    public boolean isNull() {
        return runtimeContext.isNull(instance);
    }

    public SchemaVerifier createSchemaVerifier() {
        return new SchemaVerifier(runtimeContext, this);
    }

    public DataObject getValue(List<Object> properties) {
        if (properties.isEmpty())
            return this;
        return getValue(properties.get(0)).getValue(properties.subList(1, properties.size()));
    }

    public DataObject getValue(Object property) {
        List<Object> propertyChain = schemaType.access(property).getPropertyChainBefore(schemaType);
        if (propertyChain.size() == 1 && propertyChain.get(0).equals(property))
            return new DataObject(getPropertyValue(property), runtimeContext, propertySchema(property));
        return getValue(propertyChain);
    }

    private Object getPropertyValue(Object property) {
        return isList() ? getValueFromList(property) : runtimeContext.getPropertyValue(instance, (String) property);
    }

    private Object getValueFromList(Object property) {
        if ("size".equals(property))
            return getListSize();
        if (property instanceof String)
            return getListObjects().stream().map(e -> e.getPropertyValue(property)).collect(toList());
        return getListValues().get((int) property);
    }

    private SchemaType propertySchema(Object property) {
        return isList() && property instanceof String && !"size".equals(property) ?
                schemaType.mappingAccess(property) : schemaType.access(property);
    }

    public Object firstFieldFromAlias(Object alias) {
        return schemaType.firstFieldFromAlias(alias);
    }
}
