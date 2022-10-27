package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.SortGroupNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.inspector.InspectorCache;
import com.github.leeonky.dal.runtime.inspector.InspectorContext;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.dal.runtime.CurryingMethod.createCurryingMethod;
import static java.lang.String.format;
import static java.util.Optional.of;
import static java.util.stream.Collectors.toList;
import static java.util.stream.StreamSupport.stream;

public class Data {
    private final SchemaType schemaType;
    private final DALRuntimeContext context;
    private final Object instance;
    private List<Object> listValue;
    private Comparator<Object> listComparator = SortGroupNode.NOP_COMPARATOR;

    public Data(Object instance, DALRuntimeContext context, SchemaType schemaType) {
        this.instance = instance;
        this.schemaType = schemaType;
        this.context = context.registerPropertyAccessor(instance);
    }

    public Object getInstance() {
        return instance;
    }

    public Set<Object> getFieldNames() {
        return context.findPropertyReaderNames(instance);
    }

    public boolean isList() {
        return context.isRegisteredList(instance) || (instance != null && instance.getClass().isArray());
    }

    public int getListSize() {
        return getValueList().size();
    }

    public List<Object> getValueList() {
        return listValue == null ? (listValue = stream(context.getList(instance).spliterator(), false)
                .sorted(listComparator).collect(toList())) : listValue;
    }

    public List<Data> getDataList() {
        AtomicInteger index = new AtomicInteger(0);
        return getValueList().stream().map(object -> new Data(object, context,
                schemaType.access(index.incrementAndGet()))).collect(toList());
    }

    public boolean isNull() {
        return context.isNull(instance);
    }

    public Data getValue(List<Object> propertyChain) {
        return propertyChain.isEmpty() ? this :
                getValue(propertyChain.get(0)).getValue(propertyChain.subList(1, propertyChain.size()));
    }

    public Data getValue(Object propertyChain) {
        try {
            List<Object> chain = schemaType.access(propertyChain).getPropertyChainBefore(schemaType);
            if (chain.size() == 1 && chain.get(0).equals(propertyChain))
                return new Data(getPropertyValue(propertyChain), context, propertySchema(propertyChain));
            return getValue(chain);
        } catch (IndexOutOfBoundsException ex) {
            throw new PropertyAccessException("Index out of bounds (" + ex.getMessage() + "), first index is: " + getListFirstIndex(), ex);
        } catch (Exception e) {
            throw new PropertyAccessException(format("Get property `%s` failed, property can be:\n" +
                            "  1. public field\n" +
                            "  2. public getter\n" +
                            "  3. public no args method\n" +
                            "  4. Map key value\n" +
                            "  5. customized type getter\n" +
                            "  6. static method extension\n%s%s",
                    propertyChain, e.getMessage(), listMappingMessage(this, propertyChain)), e);
        }
    }

    private String listMappingMessage(Data data, Object symbol) {
        return data.isList() ? format("\nImplicit list mapping is not allowed in current version of DAL, use `%s[]` instead",
                symbol) : "";
    }

    private Object getPropertyValue(Object property) {
        return isList() ? fetchFromList(property) : context.getPropertyValue(this, property);
    }

    private Object fetchFromList(Object property) {
        if (property instanceof String)
            return context.getPropertyValue(this, property);
        if ((int) property < 0)
            return getValueList().get(getListSize() + (int) property);
        return getValueList().get((int) property - getListFirstIndex());
    }

    public int getListFirstIndex() {
        return context.getListFirstIndex(instance);
    }

    private SchemaType propertySchema(Object property) {
        if (isList() && property instanceof String)
            return schemaType.mappingAccess(property);
        return schemaType.access(property);
    }

    public Object firstFieldFromAlias(Object alias) {
        return schemaType.firstFieldFromAlias(alias);
    }

    public Data convert(Class<?> target) {
        return new Data(context.getConverter().convert(target, instance), context, schemaType);
    }

    public Data setListComparator(Comparator<Object> listComparator) {
        this.listComparator = listComparator;
        return this;
    }

    public Data listMap(Object property) {
        return new Data(listMap(data -> data.getValue(property).getInstance()), context, propertySchema(property));
    }

    public AutoMappingList listMap(Function<Data, Object> mapper) {
        return new AutoMappingList(getListFirstIndex(), getDataList(), mapper);
    }

    public Data filter(String prefix) {
        FilteredObject filteredObject = new FilteredObject();
        getFieldNames().stream().filter(String.class::isInstance).map(String.class::cast)
                .filter(field -> field.startsWith(prefix)).forEach(fieldName ->
                        filteredObject.put(trimPrefix(prefix, fieldName), getValue(fieldName).getInstance()));
        return new Data(filteredObject, context, schemaType).setListComparator(listComparator);
    }

    private String trimPrefix(String prefix, String fieldName) {
        return fieldName.substring(prefix.length(), prefix.length() + 1).toLowerCase()
                + fieldName.substring(prefix.length() + 1);
    }

    public String inspect() {
        return new InspectorContext("root", InspectorCache.cache(), context).inspect(this);
    }

    public String dump() {
        return new InspectorContext("root", InspectorCache.cache(), context).dump(this);
    }

    public <T> T newBlockScope(Supplier<T> supplier) {
        return context.newBlockScope(this, supplier);
    }

    public Optional<CurryingMethod> currying(Object property) {
        return currying(instance, property);
    }

    private Optional<CurryingMethod> currying(Object instance, Object property) {
        List<InstanceCurryingMethod> methods = context.methodToCurrying(instance.getClass(), property).stream()
                .map(method -> createCurryingMethod(instance, method, context.getConverter())).collect(toList());
        if (!methods.isEmpty())
            return of(new CurryingMethodGroup(methods, null));
        return context.getImplicitObject(instance).flatMap(obj -> currying(obj, property));
    }

    public Data requireList(int position) {
        if (isList())
            return this;
        throw new RuntimeException(format("Invalid input value, expect a List but: %s", inspect().trim()), position);
    }

    public boolean numberNotEquals(Data another) {
        return context.getNumberType().compare((Number) instance, (Number) another.instance) != 0;
    }

    static class FilteredObject extends LinkedHashMap<String, Object> implements PartialObject {
    }
}
