package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.SortSequenceNode;

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.github.leeonky.util.BeanClass.getClassName;
import static java.util.stream.Collectors.toList;
import static java.util.stream.StreamSupport.stream;

public class Data {
    private final SchemaType schemaType;
    private final RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext;
    private final Object instance;
    private List<Object> listValue;
    private Comparator<Object> listComparator = SortSequenceNode.NOP_COMPARATOR;

    public Data(Object instance, RuntimeContextBuilder.DALRuntimeContext context, SchemaType schemaType) {
        this.instance = instance;
        this.schemaType = schemaType;
        DALRuntimeContext = context.registerPropertyAccessor(instance);
    }

    public String inspect() {
        return isNull() ? "null " : String.format("%s\n<%s>\n", getClassName(getInstance()), getInstance());
    }

    public Object getInstance() {
        return instance;
    }

    public Set<String> getFieldNames() {
        return DALRuntimeContext.findPropertyReaderNames(instance);
    }

    public boolean isList() {
        return DALRuntimeContext.isRegisteredList(instance) || (instance != null && instance.getClass().isArray());
    }

    public int getListSize() {
        return getListValues().size();
    }

    private List<Object> getListValues() {
        return listValue == null ? (listValue = stream(DALRuntimeContext.getList(instance).spliterator(), false)
                .sorted(listComparator).collect(toList())) : listValue;
    }

    public List<Data> getListObjects() {
        AtomicInteger index = new AtomicInteger(0);
        return getListValues().stream().map(object -> new Data(object, DALRuntimeContext,
                schemaType.access(index.incrementAndGet()))).collect(toList());
    }

    public boolean isNull() {
        return DALRuntimeContext.isNull(instance);
    }

    public SchemaVerifier createSchemaVerifier() {
        return new SchemaVerifier(DALRuntimeContext, this);
    }

    public Data getValue(List<Object> properties) {
        if (properties.isEmpty())
            return this;
        return getValue(properties.get(0)).getValue(properties.subList(1, properties.size()));
    }

    public Data getValue(Object property) {
        List<Object> propertyChain = schemaType.access(property).getPropertyChainBefore(schemaType);
        if (propertyChain.size() == 1 && propertyChain.get(0).equals(property))
            return new Data(getPropertyValue(property), DALRuntimeContext, propertySchema(property));
        return getValue(propertyChain);
    }

    private Object getPropertyValue(Object property) {
        return isList() ? getValueFromList(property) : DALRuntimeContext.getPropertyValue(instance, (String) property);
    }

    private Object getValueFromList(Object property) {
        if (DALRuntimeContext.isListMapping()) {
            DALRuntimeContext.endListMapping();
            return getListObjects().stream().map(e -> e.getValue(property).getInstance()).collect(toList());
        }
        if ("size".equals(property))
            return getListSize();
        if (property instanceof String) {
            try {
                return DALRuntimeContext.getPropertyValue(instance, (String) property);
            } catch (Exception e) {
                DALRuntimeContext.beginListMapping();
                return getValueFromList(property);
            }
        }
        if ((int) property < 0)
            return getListValues().get(getListSize() + (int) property);
        return getListValues().get((int) property - getListFirstIndex());
    }

    public int getListFirstIndex() {
        return DALRuntimeContext.getListFirstIndex(instance);
    }

    private SchemaType propertySchema(Object property) {
        if (isList() && property instanceof String) {
            if (!"size".equals(property))
                return schemaType.mappingAccess(property);
        }
        return schemaType.access(property);
    }

    public Object firstFieldFromAlias(Object alias) {
        return schemaType.firstFieldFromAlias(alias);
    }

    public Data convert(Class<?> target) {
        return new Data(DALRuntimeContext.getConverter().convert(target, instance), DALRuntimeContext, schemaType);
    }

    public Data setListComparator(Comparator<Object> listComparator) {
        this.listComparator = listComparator;
        return this;
    }

    public Data mapList(Object property) {
//        TODO raise error when data is not list
        return new Data(getListObjects().stream().map(data -> data.getValue(property).getInstance())
                .collect(Collectors.toCollection(AutoMappingList::new)),
                DALRuntimeContext, propertySchema(property));
    }

}
