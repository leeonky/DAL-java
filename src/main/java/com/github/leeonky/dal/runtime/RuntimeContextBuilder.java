package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Formatters;
import com.github.leeonky.interpreter.FunctionUtil;
import com.github.leeonky.interpreter.RuntimeContext;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.Converter;
import com.github.leeonky.util.Suppressor;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.*;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.lang.reflect.Modifier.PUBLIC;
import static java.lang.reflect.Modifier.STATIC;
import static java.util.Collections.emptySet;

public class RuntimeContextBuilder {
    private final ClassKeyMap<PropertyAccessor<Object>> propertyAccessors = new ClassKeyMap<>();
    private final ClassKeyMap<ListAccessor<Object>> listAccessors = new ClassKeyMap<>();
    private final Map<String, ConstructorViaSchema> constructors = new LinkedHashMap<>();
    private final Map<String, BeanClass<?>> schemas = new HashMap<>();
    private Converter converter = Converter.getInstance();
    private final Set<Method> extensionMethods = new HashSet<>();
    private final List<UserLiteralRule> userDefinedLiterals = new ArrayList<>();

    public RuntimeContextBuilder() {
        registerValueFormat(new Formatters.String())
                .registerValueFormat(new Formatters.URL())
                .registerValueFormat(new Formatters.Instant())
                .registerValueFormat(new Formatters.LocalDate())
                .registerValueFormat(new Formatters.LocalDateTime())
                .registerValueFormat(new Formatters.Enum<>())
                .registerValueFormat(new Formatters.Number())
                .registerValueFormat(new Formatters.PositiveInteger())
                .registerValueFormat(new Formatters.Integer())
                .registerValueFormat(new Formatters.PositiveNumber())
                .registerValueFormat(new Formatters.ZeroNumber())
                .registerValueFormat(new Formatters.Boolean())
                .registerSchema("List", Data::isList)
                .registerListAccessor(Iterable.class, iterable -> iterable)
                .registerListAccessor(Stream.class, stream -> stream::iterator)
                .registerPropertyAccessor(Map.class, new MapPropertyAccessor())
                .registerPropertyAccessor(AutoMappingList.class, new AutoMappingListPropertyAccessor());
    }

    public DALRuntimeContext build(Object inputValue) {
        return new DALRuntimeContext(inputValue);
    }

    public RuntimeContextBuilder registerValueFormat(Formatter<?, ?> formatter) {
        return registerValueFormat(formatter.getFormatterName(), formatter);
    }

    @SuppressWarnings("unchecked")
    public RuntimeContextBuilder registerValueFormat(String name, Formatter<?, ?> formatter) {
        constructors.put(name, o -> ((Formatter<Object, ?>) formatter).transform(o.getInstance()));
        return this;
    }

    public RuntimeContextBuilder registerSchema(Class<?> schema) {
        return registerSchema(NameStrategy.SIMPLE_NAME, schema);
    }

    public RuntimeContextBuilder registerSchema(String name, Class<?> schema) {
        schemas.put(name, BeanClass.create(schema));
        return registerSchema(name, data -> data.createSchemaVerifier().verify(schema, null, ""));
    }

    public RuntimeContextBuilder registerSchema(String name, Function<Data, Boolean> predicate) {
        constructors.put(name, (o) -> {
            if (predicate.apply(o))
                return o.getInstance();
            throw new IllegalTypeException();
        });
        return this;
    }

    @SuppressWarnings("unchecked")
    public <T> RuntimeContextBuilder registerPropertyAccessor(Class<T> type, PropertyAccessor<? extends T> propertyAccessor) {
        propertyAccessors.put(type, (PropertyAccessor<Object>) propertyAccessor);
        return this;
    }

    @SuppressWarnings("unchecked")
    public <T> RuntimeContextBuilder registerListAccessor(Class<T> type, ListAccessor<? extends T> listAccessor) {
        listAccessors.put(type, (ListAccessor<Object>) listAccessor);
        return this;
    }

    public RuntimeContextBuilder registerSchema(NameStrategy nameStrategy, Class<?> schema) {
        return registerSchema(nameStrategy.toName(schema), schema);
    }

    public RuntimeContextBuilder setConverter(Converter converter) {
        this.converter = converter;
        return this;
    }

    public RuntimeContextBuilder registerStaticMethodExtension(Class<?> staticMethodExtensionClass) {
        Stream.of(staticMethodExtensionClass.getMethods())
                .filter(RuntimeContextBuilder.this::maybeExtensionMethods)
                .forEach(extensionMethods::add);
        return this;
    }

    public Converter getConverter() {
        return converter;
    }

    public RuntimeContextBuilder registerUserDefinedLiterals(UserLiteralRule rule) {
        userDefinedLiterals.add(rule);
        return this;
    }

    private static class MapPropertyAccessor implements PropertyAccessor<Map<String, ?>> {
        @Override
        public Object getValue(Map<String, ?> instance, String name) {
            return instance.get(name);
        }

        @Override
        public Set<String> getPropertyNames(Map<String, ?> instance) {
            return instance.keySet();
        }

        @Override
        public boolean isNull(Map<String, ?> instance) {
            return instance == null;
        }
    }

    public class DALRuntimeContext implements RuntimeContext<DALRuntimeContext> {
        private final LinkedList<Data> stack = new LinkedList<>();
        private final Set<Class<?>> schemaSet;
        private final Map<Data, FlattenDataCollection> flattenDataMap;

        public DALRuntimeContext(Object inputValue) {
            schemaSet = schemas.values().stream().map(BeanClass::getType).collect(Collectors.toSet());
            stack.push(wrap(inputValue));
            flattenDataMap = new HashMap<>();
        }

        public Data getThis() {
            return stack.getFirst();
        }

        public <T> T newBlockScope(Data data, Supplier<T> supplier) {
            try {
                stack.push(data);
                return supplier.get();
            } finally {
                stack.pop();
            }
        }

        public Optional<ConstructorViaSchema> searchConstructor(String type) {
            return Optional.ofNullable(constructors.get(type));
        }

        public boolean isSchemaRegistered(Class<?> fieldType) {
            return schemaSet.contains(fieldType);
        }

        public Set<String> findPropertyReaderNames(Object instance) {
            return propertyAccessors.getData(instance).getPropertyNames(instance);
        }

        public Boolean isNull(Object instance) {
            return propertyAccessors.tryGetData(instance).map(f -> f.isNull(instance))
                    .orElseGet(() -> Objects.equals(instance, null));
        }

        public Object getPropertyValue(Data data, String name) {
            return propertyAccessors.getData(data.getInstance()).getValueByData(data, name);
        }

        @SuppressWarnings("unchecked")
        public Iterable<Object> getList(Object instance) {
            return listAccessors.tryGetData(instance).map(l -> (Iterable<Object>) l.toIterable(instance))
                    .orElseGet(() -> arrayIterable(instance));
        }

        public int getListFirstIndex(Object instance) {
            return listAccessors.tryGetData(instance).map(ListAccessor::firstIndex)
                    .orElse(0);
        }

        private Iterable<Object> arrayIterable(Object instance) {
            return () -> new Iterator<Object>() {
                private final int length = Array.getLength(instance);
                private int index = 0;

                @Override
                public boolean hasNext() {
                    return index < length;
                }

                @Override
                public Object next() {
                    return Array.get(instance, index++);
                }
            };
        }

        public boolean isRegisteredList(Object instance) {
            return listAccessors.containsType(instance);
        }

        public Converter getConverter() {
            return converter;
        }

        public Data wrap(Object instance) {
            return new Data(instance, this, SchemaType.createRoot());
        }

        public Data wrap(Object instance, String schema, boolean isList) {
            BeanClass<?> schemaBeanClass = schemas.get(schema);
            if (isList)
                schemaBeanClass = BeanClass.create(Array.newInstance(schemaBeanClass.getType(), 0).getClass());
            return new Data(instance, this, SchemaType.create(schemaBeanClass));
        }

        public <T> DALRuntimeContext registerPropertyAccessor(T instance) {
            if (!Objects.equals(instance, null) && !propertyAccessors.containsType(instance))
                propertyAccessors.put(BeanClass.getClass(instance),
                        new JavaClassPropertyAccessor<>(RuntimeContextBuilder.this, BeanClass.createFrom(instance)));
            return this;
        }

        public Optional<Result> takeUserDefinedLiteral(String token) {
            return userDefinedLiterals.stream().map(userLiteralRule -> userLiteralRule.compile(token))
                    .filter(Result::hasResult)
                    .findFirst();
        }

        public void appendFlattenProperty(Data data, Object symbol) {
            fetchFlattenData(data).map(flattenData -> flattenData.properties.add(symbol));
        }

        private Optional<FlattenData> fetchFlattenData(Data data) {
            return flattenDataMap.values().stream().map(flattenDataCollection -> flattenDataCollection.fetchFlattenData(data))
                    .filter(Objects::nonNull).findFirst();
        }

        public void setFlattenProperty(Data parent, Object symbol, Data property) {
            FlattenDataCollection flattenDataCollection = flattenDataMap.get(parent);
            if (flattenDataCollection == null)
                flattenDataCollection = fetchFlattenData(parent).map(flattenData -> flattenData.children).orElse(null);
            if (flattenDataCollection == null)
                flattenDataMap.put(parent, flattenDataCollection = new FlattenDataCollection());
            flattenDataCollection.initFlattenData(property, symbol);
        }

        public Set<String> removeFlattenProperties(Data parent) {
            FlattenDataCollection flattenDataCollection = flattenDataMap.get(parent);
            if (flattenDataCollection != null)
                return flattenDataCollection.removeFlattenProperties(parent);
            return fetchFlattenData(parent).map(flattenData -> flattenData.children.removeFlattenProperties(parent)).orElse(emptySet());
        }
    }

    static class FlattenDataCollection {
        private final Map<Data, FlattenData> collection = new HashMap<>();

        public void initFlattenData(Data instance, Object symbol) {
            collection.put(instance, new FlattenData(instance, symbol));
        }

        public FlattenData fetchFlattenData(Data data) {
            FlattenData flattenData = collection.get(data);
            if (flattenData == null)
                flattenData = collection.values().stream().map(subFlattenData -> subFlattenData.children.fetchFlattenData(data))
                        .filter(Objects::nonNull).findFirst().orElse(null);
            return flattenData;
        }

        public Set<String> removeFlattenProperties(Data data) {
            return collection.values().stream().flatMap(flattenData -> flattenData.removeFlattenProperties(data).stream())
                    .collect(Collectors.toSet());
        }
    }

    static class FlattenData {
        final Data instance;
        final Object symbol;
        final Set<Object> properties = new HashSet<>();
        final FlattenDataCollection children = new FlattenDataCollection();

        public FlattenData(Data instance, Object symbol) {
            this.instance = instance;
            this.symbol = symbol;
        }

        public Set<String> removeFlattenProperties(Data data) {
            properties.addAll(children.removeFlattenProperties(instance));
            return properties.stream().map(property -> ((Flatten) instance.getInstance())
                    .removeExpectedFields(data.getFieldNames(), symbol, property)).flatMap(Collection::stream).collect(Collectors.toSet());
        }
    }

    public Object invokeExtensionMethod(Object instance, String name) {
        Method method = FunctionUtil.oneOf(() -> findExtensionMethod(instance, name, Object::equals),
                () -> findExtensionMethod(instance, name, Class::isAssignableFrom)).orElseThrow(() ->
                new IllegalStateException(format("Method or property `%s` does not exist in `%s`", name,
                        instance.getClass().getName())));
        return Suppressor.get(() -> method.invoke(null, instance));
    }

    private Optional<Method> findExtensionMethod(Object instance, String name, BiPredicate<Class<?>, Class<?>> condition) {
        Stream<Method> methodStream = extensionMethods.stream().filter(method -> method.getName().equals(name)
                && condition.test(method.getParameterTypes()[0], instance.getClass()));
        List<Method> methods = methodStream.collect(Collectors.toList());
        if (methods.size() > 1)
            throw new IllegalStateException("Ambiguous method call:\n"
                    + methods.stream().map(Method::toString).collect(Collectors.joining("\n")));
        return methods.stream().findFirst();
    }

    private boolean maybeExtensionMethods(Method method) {
        return method.getParameterCount() == 1 && isMethod(method, STATIC) && isMethod(method, PUBLIC);
    }

    private boolean isMethod(Method method, int modifier) {
        return (modifier & method.getModifiers()) != 0;
    }

    private class AutoMappingListPropertyAccessor extends JavaClassPropertyAccessor<AutoMappingList> {
        public AutoMappingListPropertyAccessor() {
            super(RuntimeContextBuilder.this, BeanClass.create(AutoMappingList.class));
        }

        @Override
        public Object getValueByData(Data data, String name) {
            return data.mapList(name).getInstance();
        }
    }
}
