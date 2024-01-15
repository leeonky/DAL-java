package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.inspector.DumpingBuffer;

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.dal.ast.node.SortGroupNode.NOP_COMPARATOR;
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
    private DataList list;

    public Data(Object instance, DALRuntimeContext context, SchemaType schemaType) {
        this.instance = instance;
        this.schemaType = schemaType;
        this.context = context;
    }

    public Object instance() {
        return instance;
    }

    public Set<Object> fieldNames() {
        return context.findPropertyReaderNames(instance);
    }

    public boolean isList() {
        return context.isRegisteredList(instance) || (instance != null && instance.getClass().isArray());
    }

    public DataList list() {
        if (list == null) {
            if (!isList())
                throw new java.lang.RuntimeException(format("Invalid input value, expect a List but: %s", dumpAll().trim()));
            list = new DataList(context.createCollection(instance));
        }
        return list;
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
        } catch (ListMappingElementAccessException ex) {
            throw new PropertyAccessException(ex.toDalError(0).getMessage(),
                    ex.propertyAccessException().getCause());
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
        return property instanceof String ? context.getPropertyValue(this, property) :
                list().getByIndex((int) property);
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

    public Data map(Function<Object, Object> mapper) {
        return new Data(mapper.apply(instance), context, schemaType);
    }

    public Data filter(String prefix) {
        FilteredObject filteredObject = new FilteredObject();
        fieldNames().stream().filter(String.class::isInstance).map(String.class::cast)
                .filter(field -> field.startsWith(prefix)).forEach(fieldName ->
                        filteredObject.put(fieldName.substring(prefix.length()), getValue(fieldName).instance()));
        return new Data(filteredObject, context, schemaType);
    }

    public String dumpAll() {
        return DumpingBuffer.rootContext(context).dump(this).content();
    }

    public String dumpValue() {
        return DumpingBuffer.rootContext(context).dumpValue(this).content();
    }

    public <T> T execute(Supplier<T> supplier) {
        return context.pushAndExecute(this, supplier);
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

    static class FilteredObject extends LinkedHashMap<String, Object> implements PartialObject {
    }

    public class DataList extends DALCollection.Decorated<Object> {
        public DataList(DALCollection<Object> origin) {
            super(origin);
        }

        public DALCollection<Data> wraps() {
            return map((index, e) -> new Data(e, context, schemaType.access(index)));
        }

        public Data listMap(Object property) {
            return new Data(listMap(data -> data.getValue(property).instance()), context, propertySchema(property));
        }

        public AutoMappingList listMap(Function<Data, Object> mapper) {
            return new AutoMappingList(mapper, wraps());
        }

        public DataList sort(Comparator<Data> comparator) {
            if (comparator != NOP_COMPARATOR) {
                return new DataList(new CollectionDALCollection<Object>(wraps().collect().stream()
                        .sorted(comparator).map(Data::instance).collect(toList())) {
                    @Override
                    public int firstIndex() {
                        return DataList.this.firstIndex();
                    }

                    @Override
                    public boolean infinite() {
                        return DataList.this.infinite();
                    }
                });
            }
            return this;
        }

        public Data wrap() {
            return new Data(this, context, schemaType);
        }
    }
}
