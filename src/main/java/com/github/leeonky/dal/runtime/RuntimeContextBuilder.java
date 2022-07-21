package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Formatters;
import com.github.leeonky.interpreter.RuntimeContext;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.Converter;
import com.github.leeonky.util.NumberType;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.time.*;
import java.util.*;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.lang.reflect.Modifier.PUBLIC;
import static java.lang.reflect.Modifier.STATIC;
import static java.util.Arrays.stream;
import static java.util.Collections.emptySet;

public class RuntimeContextBuilder {
    private final ClassKeyMap<PropertyAccessor<Object>> propertyAccessors = new ClassKeyMap<>();
    private final ClassKeyMap<ListAccessor<Object>> listAccessors = new ClassKeyMap<>();
    private final ClassKeyMap<Function<Object, Object>> objectImplicitMapper = new ClassKeyMap<>();
    private final Map<String, ConstructorViaSchema> valueConstructors = new LinkedHashMap<>();
    private final Map<String, BeanClass<?>> schemas = new HashMap<>();
    //    TODO to be remove
    private final Set<Method> extensionMethods = new HashSet<>();
    //    TODO rename
    private final Set<Method> extensionMethods2 = new HashSet<>();
    private final List<UserLiteralRule> userDefinedLiterals = new ArrayList<>();
    private final NumberType numberType = new NumberType();
    private final ClassKeyMap<Function<Object, String>> valueDumpers = new ClassKeyMap<>();
    private final ClassKeyMap<Function<Object, Map<String, Object>>> objectDumpers = new ClassKeyMap<>();
    private final Map<Method, Function<List<Object>, List<Object>>> curryingMethodArgRanges = new HashMap<>();
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
                .registerPropertyAccessor(CurryingMethod.class, new CurryingMethodPropertyAccessor())
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

    static Optional<Method> findMethodToCurrying(Class<?> type, Object property) {
        return stream(type.getMethods()).filter(method -> Modifier.isPublic(method.getModifiers())
                        && !Modifier.isStatic(method.getModifiers()))
                .filter(method -> method.getName().equals(property))
                .max(Comparator.comparingInt(method -> method.getParameters().length));
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
        Stream.of(staticMethodExtensionClass.getMethods())
                .filter(method -> method.getParameterCount() >= 1
                        && (STATIC & method.getModifiers()) != 0
                        && (PUBLIC & method.getModifiers()) != 0)
                .forEach(extensionMethods2::add);
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

    private boolean maybeExtensionMethods(Method method) {
        return method.getParameterCount() == 1
                && (STATIC & method.getModifiers()) != 0
                && (PUBLIC & method.getModifiers()) != 0;
    }

    public RuntimeContextBuilder registerCurryingMethodRange(Class<?> type, String methodName,
                                                             Function<List<Object>, List<Object>> range) {
        findMethodToCurrying(type, methodName).ifPresent(method -> curryingMethodArgRanges.put(method, range));
        return this;
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
            try {
                return propertyAccessors.getData(data.getInstance()).getValueByData(data, property);
            } catch (InvalidPropertyException e) {
                CurryingMethod curryingMethod = data.currying(property);
                if (curryingMethod != null)
                    return curryingMethod.resolve();
//                TODO return value when currying is method call
                throw e;
            }
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

        //        TODO refactor
        public Optional<Method> findStaticMethodToCurrying(Object instance, Object property) {
            return Optional.ofNullable(getMethod(instance, property, Object::equals)
                    .orElseGet(() -> getMethod(instance, property, Class::isAssignableFrom).orElse(null)));
        }

        //        TODO refactor
        private Optional<Method> getMethod(Object instance, Object property, BiPredicate<Class<?>, Class<?>> condition) {
            List<Method> collect = extensionMethods2.stream()
                    .filter(method -> method.getName().equals(property))
                    .filter(method -> condition.test(method.getParameters()[0].getType(), instance.getClass()))
                    .sorted(Comparator.comparingInt(Method::getParameterCount).reversed()).collect(Collectors.toList());
            if (collect.size() > 1) {
                if (collect.get(0).getParameterCount() != collect.get(1).getParameterCount())
                    return collect.stream().findFirst();
                throw new InvalidPropertyException("Ambiguous method call:\n"
                        + collect.stream().map(Method::toString).collect(Collectors.joining("\n")));
            }
            return collect.stream().findFirst();
        }

        public Optional<Object> getImplicitObject(Object obj) {
            return objectImplicitMapper.tryGetData(obj).map(mapper -> mapper.apply(obj));
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

    private class CurryingMethodPropertyAccessor extends JavaClassPropertyAccessor<CurryingMethod> {
        public CurryingMethodPropertyAccessor() {
            super(RuntimeContextBuilder.this, BeanClass.create(CurryingMethod.class));
        }

        @Override
        public Object getValue(CurryingMethod curryingMethod, Object property) {
            return curryingMethod.call(property, getConverter());
        }

        @Override
        public Set<Object> getPropertyNames(CurryingMethod curryingMethod) {
            Function<List<Object>, List<Object>> listListFunction = curryingMethodArgRanges.get(curryingMethod.getMethod());
            if (listListFunction != null)
                return new LinkedHashSet<>(listListFunction.apply(curryingMethod.getArgs()));
            System.err.printf("No arg range for %s, give the range or use `:`%n", curryingMethod.parameterInfo());
            return emptySet();
        }
    }
}
