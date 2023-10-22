package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;
import com.github.leeonky.dal.ast.node.SortGroupNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.inspector.DumpingBuffer;

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static com.github.leeonky.dal.runtime.CurryingMethod.createCurryingMethod;
import static com.github.leeonky.util.Classes.named;
import static java.lang.String.format;
import static java.util.Optional.of;
import static java.util.stream.Collectors.toList;

//TODO use generic
public class Data {
    private final SchemaType schemaType;
    private final DALRuntimeContext context;
    private final Object instance;
    private ListWrapper listWrapper;
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

    private ListWrapper listWrapper() {
        if (listWrapper == null)
            listWrapper = context.getListWrapper(instance, listComparator);
        return listWrapper;
    }

    public int sizeOfList() {
        return listWrapper().size();
    }

    public Stream<IndexedElement<Data>> indexedListData() {
        return listWrapper().indexedList().map(e ->
                new IndexedElement<>(e.index(), new Data(e.value(), context, schemaType.access(e.index()))));
    }

    public Stream<Data> listData() {
        return indexedListData().map(IndexedElement::value);
    }

    public Stream<Object> list() {
        return listData().map(Data::getInstance);
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
            throw new PropertyAccessException(ex.getMessage(), ex);
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
        return property instanceof String ? context.getPropertyValue(this, property) : listWrapper().getByIndex((int) property);
    }

    @Deprecated
    public int getListFirstIndex() {
        return listWrapper().firstIndex();
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
        listWrapper = null;
        return this;
    }

    public Data listMap(Object property) {
        return new Data(listMap(data -> data.getValue(property).getInstance()), context, propertySchema(property));
    }

    public AutoMappingList listMap(Function<Data, Object> mapper) {
        return new AutoMappingList(getListFirstIndex(), listData().collect(toList()), mapper);
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

    public String dumpAll() {
        return DumpingBuffer.rootContext(context).dump(this).content();
    }

    public String dumpValue() {
        return DumpingBuffer.rootContext(context).dumpValue(this).content();
    }

    public <T> T newBlockScope(Supplier<T> supplier) {
        return context.newBlockScope(this, supplier);
    }

    public Optional<CurryingMethod> currying(Object property) {
        return currying(instance, property);
    }

    private Optional<CurryingMethod> currying(Object instance, Object property) {
        List<InstanceCurryingMethod> methods = context.methodToCurrying(named(instance.getClass()), property).stream()
                .map(method -> createCurryingMethod(instance, method, context.getConverter())).collect(toList());
        if (!methods.isEmpty())
            return of(new CurryingMethodGroup(methods, null));
        return context.getImplicitObject(instance).flatMap(obj -> currying(obj, property));
    }

    public Data requireList(int position) {
        if (!isList())
            throw new RuntimeException(format("Invalid input value, expect a List but: %s", dumpAll().trim()), position);
        try {
            list();
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), position, e);
        }
        return this;
    }

    public boolean isNullWithPosition(int position) {
        boolean isNull;
        try {
            isNull = isNull();
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), position, e);
        }
        return isNull;
    }

    static class FilteredObject extends LinkedHashMap<String, Object> implements PartialObject {
    }
}
