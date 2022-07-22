package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.DALNode;
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
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.interpreter.FunctionUtil.oneOf;
import static java.lang.String.format;
import static java.lang.reflect.Modifier.PUBLIC;
import static java.lang.reflect.Modifier.STATIC;
import static java.util.Arrays.stream;
import static java.util.Collections.emptySet;
import static java.util.Comparator.comparingInt;
import static java.util.stream.Collectors.toList;

public class RuntimeContextBuilder {
    private final ClassKeyMap<PropertyAccessor<Object>> propertyAccessors = new ClassKeyMap<>();
    private final ClassKeyMap<ListAccessor<Object>> listAccessors = new ClassKeyMap<>();
    private final ClassKeyMap<Function<Object, Object>> objectImplicitMapper = new ClassKeyMap<>();
    private final Map<String, ConstructorViaSchema> valueConstructors = new LinkedHashMap<>();
    private final Map<String, BeanClass<?>> schemas = new HashMap<>();
    private final Set<Method> extensionMethods = new HashSet<>();
    private final Map<Object, Function<MetaData, Object>> metaProperties = new HashMap<>();
    private final List<UserLiteralRule> userDefinedLiterals = new ArrayList<>();
    private final NumberType numberType = new NumberType();
    private final ClassKeyMap<Function<Object, String>> valueDumpers = new ClassKeyMap<>();
    private final ClassKeyMap<Function<Object, Map<String, Object>>> objectDumpers = new ClassKeyMap<>();
    private final Map<Method, BiFunction<Object, List<Object>, List<Object>>> curryingMethodArgRanges = new HashMap<>();
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
                .registerPropertyAccessor(AutoMappingList.class, new AutoMappingListPropertyAccessor(this))
                .registerPropertyAccessor(CurryingMethod.class, new CurryingMethodPropertyAccessor(this))
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

        registerMetaProperty("size", BuildInMetaProperty::size);
    }

    public RuntimeContextBuilder registerMetaProperty(Object property, Function<MetaData, Object> function) {
        metaProperties.put(property, function);
        return this;
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
        Stream.of(staticMethodExtensionClass.getMethods()).filter(method -> method.getParameterCount() >= 1
                        && (STATIC & method.getModifiers()) != 0
                        && (PUBLIC & method.getModifiers()) != 0)
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

    public RuntimeContextBuilder registerCurryingMethodRange(Class<?> type, String methodName,
                                                             BiFunction<Object, List<Object>, List<Object>> range) {
        methodToCurrying(type, methodName).ifPresent(method -> curryingMethodArgRanges.put(method, range));
        return this;
    }

    private Optional<Method> methodToCurrying(Class<?> type, Object methodName) {
        return oneOf(() -> instanceMethodToCurrying(type, methodName),
                () -> staticMethodToCurrying(type, methodName, Object::equals),
                () -> staticMethodToCurrying(type, methodName, Class::isAssignableFrom));
    }

    static Optional<Method> instanceMethodToCurrying(Class<?> type, Object property) {
        return getMaxParameterCountMethod(stream(type.getMethods())
                .filter(method -> Modifier.isPublic(method.getModifiers()) && !Modifier.isStatic(method.getModifiers()))
                .filter(method -> method.getName().equals(property)));
    }

    private static Optional<Method> getMaxParameterCountMethod(Stream<Method> methodStream) {
        List<Method> methods = methodStream.sorted(comparingInt(Method::getParameterCount)).collect(toList());
        if (methods.size() > 1 && methods.get(0).getParameterCount() == methods.get(1).getParameterCount())
            throw new InvalidPropertyException("Ambiguous method call:\n"
                    + methods.stream().map(Method::toString).collect(Collectors.joining("\n")));
        return methods.stream().findFirst();
    }

    private Optional<Method> staticMethodToCurrying(Class<?> type, Object property,
                                                    BiPredicate<Class<?>, Class<?>> condition) {
        return getMaxParameterCountMethod(extensionMethods.stream().filter(method -> method.getName().equals(property))
                .filter(method -> condition.test(method.getParameters()[0].getType(), type)));
    }

    BiFunction<Object, List<Object>, List<Object>> fetchCurryingMethodArgRange(Method method) {
        return curryingMethodArgRanges.get(method);
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
                return data.currying(property).orElseThrow(() -> e).resolve();
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

        public Optional<Object> getImplicitObject(Object obj) {
            return objectImplicitMapper.tryGetData(obj).map(mapper -> mapper.apply(obj));
        }

        public Optional<Method> methodToCurrying(Class<?> type, Object methodName) {
            return RuntimeContextBuilder.this.methodToCurrying(type, methodName);
        }

        public Data metaProperty(DALNode metaDataNode, DALNode property) {
            Function<MetaData, Object> function = metaProperties.get(property.getRootSymbolName());
            if (function == null)
                throw new RuntimeException(format("Meta property `%s` not found", property.getRootSymbolName()), property.getPositionBegin());
            return wrap(function.apply(new MetaData(metaDataNode, property, this)));
        }
    }
}
