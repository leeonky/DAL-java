package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Formatters;
import com.github.leeonky.interpreter.FunctionUtil;
import com.github.leeonky.interpreter.RuntimeContext;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.Converter;
import com.github.leeonky.util.NumberType;
import com.github.leeonky.util.Suppressor;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.time.*;
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
    private final ClassKeyMap<Function<Object, Object>> objectImplicitMapper = new ClassKeyMap<>();
    private final Map<String, ConstructorViaSchema> valueConstructors = new LinkedHashMap<>();
    private final Map<String, BeanClass<?>> schemas = new HashMap<>();
    private final Set<Method> extensionMethods = new HashSet<>();
    private final List<UserLiteralRule> userDefinedLiterals = new ArrayList<>();
    private final NumberType numberType = new NumberType();
    private final ClassKeyMap<Function<Object, String>> valueDumpers = new ClassKeyMap<>();
    private final ClassKeyMap<Function<Object, Map<String, Object>>> objectDumpers = new ClassKeyMap<>();
    private Converter converter = Converter.getInstance();

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
                .registerPropertyAccessor(AutoMappingList.class, new AutoMappingListPropertyAccessor())
        ;

        registerValueDumper(String.class, RuntimeContextBuilder::dumpString)
                .registerValueDumper(Number.class, Object::toString)
                .registerValueDumper(Boolean.class, Object::toString)
                .registerValueDumper(boolean.class, Object::toString)
        ;

        registerObjectDumper(UUID.class, Object::toString)
                .registerObjectDumper(Instant.class, Object::toString)
                .registerObjectDumper(Date.class, date -> date.toInstant().toString())
                .registerObjectDumper(LocalTime.class, LocalTime::toString)
                .registerObjectDumper(LocalDate.class, LocalDate::toString)
                .registerObjectDumper(LocalDateTime.class, LocalDateTime::toString)
                .registerObjectDumper(OffsetDateTime.class, OffsetDateTime::toString)
                .registerObjectDumper(ZonedDateTime.class, ZonedDateTime::toString)
                .registerObjectDumper(YearMonth.class, YearMonth::toString)
                .registerObjectDumper(Class.class, Class::getName)
        ;
    }

    private static String dumpString(Object o) {
        return "\"" + o.toString().replace("\\", "\\\\").replace("\t", "\\t").replace("\b", "\\b").replace("\n", "\\n")
                .replace("\r", "\\r").replace("\f", "\\f").replace("'", "\\'").replace("\"", "\\\"") + "\"";
    }

    @SuppressWarnings("unchecked")
    public <T> RuntimeContextBuilder registerValueDumper(Class<T> key, Function<T, String> toString) {
        valueDumpers.put(key, obj -> toString.apply((T) obj));
        return this;
    }

    @SuppressWarnings("unchecked")
    public <T> RuntimeContextBuilder registerObjectDumper(Class<T> type, Function<T, String> toString) {
        objectDumpers.put(type, obj -> new LinkedHashMap<String, Object>() {{
            put("__type", obj.getClass().getName());
            put("__value", toString.apply((T) obj));
        }});
        return this;
    }

    public DALRuntimeContext build(Object inputValue) {
        return new DALRuntimeContext(inputValue);
    }

    public RuntimeContextBuilder registerValueFormat(Formatter<?, ?> formatter) {
        return registerValueFormat(formatter.getFormatterName(), formatter);
    }

    @SuppressWarnings("unchecked")
    public RuntimeContextBuilder registerValueFormat(String name, Formatter<?, ?> formatter) {
        valueConstructors.put(name, o -> ((Formatter<Object, ?>) formatter).transform(o.getInstance()));
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
        valueConstructors.put(name, (o) -> {
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

    public RuntimeContextBuilder registerStaticMethodExtension(Class<?> staticMethodExtensionClass) {
        Stream.of(staticMethodExtensionClass.getMethods())
                .filter(RuntimeContextBuilder.this::maybeExtensionMethods)
                .forEach(extensionMethods::add);
        return this;
    }

    @SuppressWarnings("unchecked")
    public <T> RuntimeContextBuilder registerImplicitData(Class<T> type, Function<T, Object> mapper) {
        objectImplicitMapper.put(type, (Function) mapper);
        return this;
    }

    public Converter getConverter() {
        return converter;
    }

    public RuntimeContextBuilder setConverter(Converter converter) {
        this.converter = converter;
        return this;
    }

    public RuntimeContextBuilder registerUserDefinedLiterals(UserLiteralRule rule) {
        userDefinedLiterals.add(rule);
        return this;
    }

    public Object invokeExtensionMethod(Object instance, String name, String typeName) {
        Optional<Method> optionalMethod = FunctionUtil.oneOf(() -> findExtensionMethod(instance, name, Object::equals),
                () -> findExtensionMethod(instance, name, Class::isAssignableFrom));
        if (optionalMethod.isPresent())
            return Suppressor.get(() -> optionalMethod.get().invoke(null, instance));
        Optional<Function<Object, Object>> mapper = objectImplicitMapper.tryGetData(instance);
        if (mapper.isPresent())
            return invokeExtensionMethod(mapper.get().apply(instance), name, typeName);
        throw new IllegalStateException(format("Method or property `%s` does not exist in `%s`", name, typeName));
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

    private static class MapPropertyAccessor implements PropertyAccessor<Map<?, ?>> {
        @Override
        public Object getValue(Map<?, ?> instance, Object property) {
            return instance.get(property);
        }

        @Override
        public Set<Object> getPropertyNames(Map<?, ?> instance) {
            return new LinkedHashSet<>(instance.keySet());
        }

        @Override
        public boolean isNull(Map<?, ?> instance) {
            return instance == null;
        }
    }

    static class PartialPropertyStack {
        private final Map<Data, PartialProperties> partials = new HashMap<>();

        public void setupPartialProperties(Object prefix, Data partial) {
            partials.put(partial, new PartialProperties(prefix, partial));
        }

        public PartialProperties fetchPartialProperties(Data instance) {
            PartialProperties partialProperties = partials.get(instance);
            if (partialProperties == null)
                partialProperties = partials.values().stream()
                        .map(sub -> sub.partialPropertyStack.fetchPartialProperties(instance))
                        .filter(Objects::nonNull).findFirst().orElse(null);
            return partialProperties;
        }

        public Set<String> collectPartialProperties(Data data) {
            return partials.values().stream().flatMap(partialProperties ->
                    partialProperties.collectPartialProperties(data).stream()).collect(Collectors.toSet());
        }
    }

    static class PartialProperties {
        final Object prefix;
        final Data partialData;
        final Set<Object> postfixes = new LinkedHashSet<>();
        final PartialPropertyStack partialPropertyStack = new PartialPropertyStack();

        public PartialProperties(Object prefix, Data partialData) {
            this.prefix = prefix;
            this.partialData = partialData;
        }

        public Set<String> collectPartialProperties(Data data) {
            postfixes.addAll(partialPropertyStack.collectPartialProperties(partialData));
            return postfixes.stream().map(property -> ((PartialObject) partialData.getInstance())
                            .removeExpectedField(data.getFieldNames(), prefix, property))
                    .filter(Optional::isPresent).map(Optional::get).collect(Collectors.toSet());
        }

        public boolean appendPartialProperties(Object symbol) {
            return postfixes.add(symbol);
        }
    }

    public class DALRuntimeContext implements RuntimeContext<DALRuntimeContext> {
        private final LinkedList<Data> stack = new LinkedList<>();
        private final Set<Class<?>> schemaSet;
        private final Map<Data, PartialPropertyStack> partialPropertyStacks;

        public DALRuntimeContext(Object inputValue) {
            schemaSet = schemas.values().stream().map(BeanClass::getType).collect(Collectors.toSet());
            stack.push(wrap(inputValue));
            partialPropertyStacks = new HashMap<>();
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

        public Optional<ConstructorViaSchema> searchValueConstructor(String type) {
            return Optional.ofNullable(valueConstructors.get(type));
        }

        public boolean isSchemaRegistered(Class<?> fieldType) {
            return schemaSet.contains(fieldType);
        }

        public Set<Object> findPropertyReaderNames(Object instance) {
            return propertyAccessors.getData(instance).getPropertyNames(instance);
        }

        public Boolean isNull(Object instance) {
            return propertyAccessors.tryGetData(instance).map(f -> f.isNull(instance))
                    .orElseGet(() -> Objects.equals(instance, null));
        }

        public Object getPropertyValue(Data data, Object property) {
            return propertyAccessors.getData(data.getInstance()).getValueByData(data, property);
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
            return listAccessors.tryGetData(instance).map(listAccessor -> listAccessor.isList(instance)).orElse(false);
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

        public void appendPartialPropertyReference(Data data, Object symbol) {
            fetchPartialProperties(data).map(partialProperties -> partialProperties.appendPartialProperties(symbol));
        }

        private Optional<PartialProperties> fetchPartialProperties(Data data) {
            return partialPropertyStacks.values().stream().map(partialPropertyStack ->
                    partialPropertyStack.fetchPartialProperties(data)).filter(Objects::nonNull).findFirst();
        }

        public void initPartialPropertyStack(Data instance, Object prefix, Data partial) {
            partialPropertyStacks.computeIfAbsent(instance, _key -> fetchPartialProperties(instance)
                    .map(partialProperties -> partialProperties.partialPropertyStack)
                    .orElseGet(PartialPropertyStack::new)).setupPartialProperties(prefix, partial);
        }

        //                    TODO object key *********************
        public Set<String> collectPartialProperties(Data instance) {
            PartialPropertyStack partialPropertyStack = partialPropertyStacks.get(instance);
            if (partialPropertyStack != null)
                return partialPropertyStack.collectPartialProperties(instance);
            return fetchPartialProperties(instance).map(partialProperties ->
                    partialProperties.partialPropertyStack.collectPartialProperties(instance)).orElse(emptySet());
        }

        public NumberType getNumberType() {
            return numberType;
        }

        public Optional<Function<Object, String>> fetchSingleDumper(Object instance) {
            return valueDumpers.tryGetData(instance);
        }

        public Optional<Function<Object, Map<String, Object>>> fetchObjectDumper(Object instance) {
            return objectDumpers.tryGetData(instance);
        }
    }

    private class AutoMappingListPropertyAccessor extends JavaClassPropertyAccessor<AutoMappingList> {
        public AutoMappingListPropertyAccessor() {
            super(RuntimeContextBuilder.this, BeanClass.create(AutoMappingList.class));
        }

        @Override
        public Object getValueByData(Data data, Object property) {
            return data.mapList(property).getInstance();
        }
    }
}
