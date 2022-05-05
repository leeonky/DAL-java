package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.SortSequenceNode;

import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.github.leeonky.util.BeanClass.getClassName;
import static java.lang.String.format;
import static java.util.Collections.nCopies;
import static java.util.stream.Collectors.toList;
import static java.util.stream.StreamSupport.stream;

public class Data {
    private final SchemaType schemaType;
    private final RuntimeContextBuilder.DALRuntimeContext dalRuntimeContext;
    private final Object instance;
    private List<Object> listValue;
    private Comparator<Object> listComparator = SortSequenceNode.NOP_COMPARATOR;

    public Data(Object instance, RuntimeContextBuilder.DALRuntimeContext context, SchemaType schemaType) {
        this.instance = instance;
        this.schemaType = schemaType;
        dalRuntimeContext = context.registerPropertyAccessor(instance);
    }

    public String inspect() {
        return isNull() ? "null " : format("%s\n<%s>\n", getClassName(getInstance()), getInstance());
    }

    public Object getInstance() {
        return instance;
    }

    public Set<String> getFieldNames() {
        return dalRuntimeContext.findPropertyReaderNames(instance);
    }

    public boolean isList() {
        return dalRuntimeContext.isRegisteredList(instance) || (instance != null && instance.getClass().isArray());
    }

    public int getListSize() {
        return getListValues().size();
    }

    private List<Object> getListValues() {
        return listValue == null ? (listValue = stream(dalRuntimeContext.getList(instance).spliterator(), false)
                .sorted(listComparator).collect(toList())) : listValue;
    }

    public List<Data> getListObjects() {
        AtomicInteger index = new AtomicInteger(0);
        return getListValues().stream().map(object -> new Data(object, dalRuntimeContext,
                schemaType.access(index.incrementAndGet()))).collect(toList());
    }

    public boolean isNull() {
        return dalRuntimeContext.isNull(instance);
    }

    public SchemaVerifier createSchemaVerifier() {
        return new SchemaVerifier(dalRuntimeContext, this);
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
                return new Data(getPropertyValue(property), dalRuntimeContext, propertySchema(property));
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
        return isList() ? getValueFromList(property) : dalRuntimeContext.getPropertyValue(this, (String) property);
    }

    private Object getValueFromList(Object property) {
        if ("size".equals(property))
            return getListSize();
        if (property instanceof String) {
            return dalRuntimeContext.getPropertyValue(this, (String) property);
        }
        if ((int) property < 0)
            return getListValues().get(getListSize() + (int) property);
        return getListValues().get((int) property - getListFirstIndex());
    }

    public int getListFirstIndex() {
        return dalRuntimeContext.getListFirstIndex(instance);
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
        return new Data(dalRuntimeContext.getConverter().convert(target, instance), dalRuntimeContext, schemaType);
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
        }}, dalRuntimeContext, propertySchema(property));
    }

    public Data filter(String prefix) {
        FilteredObject filteredObject = new FilteredObject();
        getFieldNames().stream().filter(fieldName -> fieldName.startsWith(prefix))
                .forEach(fieldName -> filteredObject.put(trimPrefix(prefix, fieldName), getValue(fieldName).getInstance()));
        return new Data(filteredObject, dalRuntimeContext, schemaType).setListComparator(listComparator);
    }

    private String trimPrefix(String prefix, String fieldName) {
        return fieldName.substring(prefix.length(), prefix.length() + 1).toLowerCase()
                + fieldName.substring(prefix.length() + 1);
    }

    public Data wrapThis() {
        return new Data(new ThisObject(this), dalRuntimeContext, schemaType);
    }

    public String dump() {
        return dump(0);
    }

    private String dump(int indent) {
        String indentation = indent(indent);
        if (isNull())
            return indentation + "null";
        if (isList()) {
            List<Data> listObjects = getListObjects();
            if (listObjects.isEmpty())
                return indentation + "[]";
            return listObjects.stream().map(data -> data.dump(indent + 2))
                    .collect(Collectors.joining(",\n", indentation + "[\n", "\n" + indentation + "]"));
        }
        return dalRuntimeContext.fetchSingleDumper(instance).map(dumper -> indentation + dumper.apply(instance))
                .orElseThrow(IllegalStateException::new);
    }

    private String indent(int indent) {
        return String.join("", nCopies(indent, " "));
    }

    private static class FilteredObject extends LinkedHashMap<String, Object> implements Flatten {
    }
}
