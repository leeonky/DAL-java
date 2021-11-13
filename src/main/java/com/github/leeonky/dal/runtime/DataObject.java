package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.SequenceNode;

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static com.github.leeonky.util.BeanClass.getClassName;
import static java.util.stream.Collectors.toList;
import static java.util.stream.StreamSupport.stream;

public class DataObject {
    private final SchemaType schemaType;
    private final RuntimeContextBuilder.RuntimeContext runtimeContext;
    private final Object instance;
    private List<Object> listValue;
    private Comparator<Object> listComparator = SequenceNode.NOP_COMPARATOR;

    public DataObject(Object instance, RuntimeContextBuilder.RuntimeContext context, SchemaType schemaType) {
        this.instance = instance;
        this.schemaType = schemaType;
        runtimeContext = context.registerPropertyAccessor(instance);
    }

    public String inspect() {
        return isNull() ? " null " : String.format(" %s\n<%s>\n", getClassName(getInstance()), getInstance());
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

    public int getListSize() {
        return getListValues().size();
    }

    private List<Object> getListValues() {
        return listValue == null ? (listValue = stream(runtimeContext.getList(instance).spliterator(), false)
                .sorted(listComparator).collect(toList())) : listValue;
    }

    public List<DataObject> getListObjects() {
        AtomicInteger index = new AtomicInteger(0);
        return getListValues().stream().map(object -> new DataObject(object, runtimeContext,
                schemaType.access(index.incrementAndGet()))).collect(toList());
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
        if (runtimeContext.isListMapping()) {
            runtimeContext.endListMapping();
            return getListObjects().stream().map(e -> e.getPropertyValue(property)).collect(toList());
        }
        if ("size".equals(property))
            return getListSize();
        if ("@".equals(property)) {
            runtimeContext.beginListMapping();
            return instance;
        }
        if (property instanceof String) {
            try {
                return runtimeContext.getPropertyValue(instance, (String) property);
            } catch (Exception e) {
                runtimeContext.beginListMapping();
                return getValueFromList(subProperty((String) property));
            }
        }
        if ((int) property < 0)
            return getListValues().get(getListSize() + (int) property);
        return getListValues().get((int) property - runtimeContext.getListFirstIndex(instance));
    }

    private Object subProperty(String property) {
        return property.replace("@size", "size");
    }

    private SchemaType propertySchema(Object property) {
        if (isList() && property instanceof String) {
            if ("@".equals(property))
                return schemaType;
            if (!"size".equals(property))
                return schemaType.mappingAccess(property);
        }
        return schemaType.access(property);
    }

    public Object firstFieldFromAlias(Object alias) {
        return schemaType.firstFieldFromAlias(alias);
    }

    public DataObject convert(Class<?> target) {
        return new DataObject(runtimeContext.getConverter().convert(target, instance), runtimeContext, schemaType);
    }

    public DataObject setListComparator(Comparator<Object> listComparator) {
        this.listComparator = listComparator;
        return this;
    }
}
