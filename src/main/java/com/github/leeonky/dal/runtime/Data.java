package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.SortSequenceNode;

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static com.github.leeonky.util.BeanClass.getClassName;
import static java.lang.String.format;
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
        return isNull() ? "null " : format("%s\n<%s>\n", getClassName(getInstance()), getInstance());
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
        try {
            List<Object> propertyChain = schemaType.access(property).getPropertyChainBefore(schemaType);
            if (propertyChain.size() == 1 && propertyChain.get(0).equals(property))
                return new Data(getPropertyValue(property), DALRuntimeContext, propertySchema(property));
            return getValue(propertyChain);
        } catch (IndexOutOfBoundsException ex) {
            throw new PropertyAccessException(property, "Index out of bounds (" + ex.getMessage() + ")");
        } catch (Exception e) {
            throw new PropertyAccessException(property, format("Get property `%s` failed, property can be:\n" +
                    "  1. public field\n" +
                    "  2. public getter\n" +
                    "  3. public no args method\n" +
                    "  4. Map key value\n" +
                    "  5. customized type getter\n" +
                    "  6. static method extension\n%s%s", property, e.getMessage(), listMappingMessage(this, property)));
        }
    }

    private String listMappingMessage(Data data, Object symbol) {
        return data.isList() ? format("\nImplicit list mapping is not allowed in current version of DAL, use `%s[]` instead",
                symbol) : "";
    }

    private Object getPropertyValue(Object property) {
        return isList() ? getValueFromList(property) : DALRuntimeContext.getPropertyValue(this, (String) property);
    }

    private Object getValueFromList(Object property) {
        if ("size".equals(property))
            return getListSize();
        if (property instanceof String) {
            return DALRuntimeContext.getPropertyValue(this, (String) property);
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
        return new Data(new AutoMappingList() {{
            List<Data> list = getListObjects();
            for (int i = 0; i < list.size(); i++) {
                try {
                    add(list.get(i).getValue(property).getInstance());
                } catch (PropertyAccessException e) {
                    throw new ElementAccessException(i, e);
                }
            }
        }}, DALRuntimeContext, propertySchema(property));
    }

}
