package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.runtime.schema.Expect;
import com.github.leeonky.dal.type.ExtensionName;
import com.github.leeonky.dal.type.Schema;
import com.github.leeonky.interpreter.RuntimeContext;
import com.github.leeonky.interpreter.SyntaxException;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.Converter;
import com.github.leeonky.util.InvocationException;
import com.github.leeonky.util.NumberType;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.dal.runtime.schema.Actual.actual;
import static com.github.leeonky.dal.runtime.schema.Verification.expect;
import static com.github.leeonky.util.BeanClass.create;
import static java.lang.String.format;
import static java.lang.reflect.Modifier.STATIC;
import static java.util.Arrays.stream;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.joining;

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
    private final Map<String, TextBlockAttribute> textAttributeMap = new LinkedHashMap<>();
    private Converter converter = Converter.getInstance();

    public RuntimeContextBuilder registerMetaProperty(Object property, Function<MetaData, Object> function) {
        metaProperties.put(property, function);
        return this;
    }

    public RuntimeContextBuilder registerTextBlockAttribute(String name, TextBlockAttribute attribute) {
        textAttributeMap.put(name, attribute);
        return this;
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
        valueConstructors.put(name, (o, c) -> ((Formatter<Object, ?>) formatter).transform(o.getInstance()));
        return this;
    }

    public RuntimeContextBuilder registerSchema(Class<? extends Schema> schema) {
        return registerSchema(NameStrategy.SIMPLE_NAME, schema);
    }

    @SuppressWarnings("unchecked")
    public RuntimeContextBuilder registerSchema(String name, Class<? extends Schema> schema) {
        schemas.put(name, create(schema));
        return registerSchema(name, (data, context) ->
                expect(new Expect(create((Class) schema), null)).verify(context, actual(data)));
    }

    public RuntimeContextBuilder registerSchema(String name, BiFunction<Data, DALRuntimeContext, Boolean> predicate) {
        valueConstructors.put(name, (o, context) -> {
            if (predicate.apply(o, context))
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

    public RuntimeContextBuilder registerSchema(NameStrategy nameStrategy, Class<? extends Schema> schema) {
        return registerSchema(nameStrategy.toName(schema), schema);
    }

    public RuntimeContextBuilder registerStaticMethodExtension(Class<?> staticMethodExtensionClass) {
        Stream.of(staticMethodExtensionClass.getMethods()).filter(method -> method.getParameterCount() >= 1
                && (STATIC & method.getModifiers()) != 0).forEach(extensionMethods::add);
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

    public RuntimeContextBuilder registerCurryingMethodRange(Method method,
                                                             BiFunction<Object, List<Object>, List<Object>> range) {
        curryingMethodArgRanges.put(method, range);
        return this;
    }

    private Set<Method> methodToCurrying(Class<?> type, Object methodName) {
        return Stream.of(stream(type.getMethods()).filter(method -> !Modifier.isStatic(method.getModifiers()))
                                .filter(method -> method.getName().equals(methodName)),
                        staticMethodsToCurrying(type, methodName, Object::equals),
                        staticMethodsToCurrying(type, methodName, Class::isAssignableFrom))
                .flatMap(Function.identity()).collect(Collectors.toCollection(LinkedHashSet::new));
    }

    private Stream<Method> staticMethodsToCurrying(Class<?> type, Object property,
                                                   BiPredicate<Class<?>, Class<?>> condition) {
        return extensionMethods.stream()
                .filter(method -> staticExtensionMethodName(method).equals(property))
                .filter(method -> condition.test(method.getParameters()[0].getType(), type));
    }

    static String staticExtensionMethodName(Method method) {
        ExtensionName extensionName = method.getAnnotation(ExtensionName.class);
        return extensionName != null ? extensionName.value() : method.getName();
    }

    BiFunction<Object, List<Object>, List<Object>> fetchCurryingMethodArgRange(Method method) {
        return curryingMethodArgRanges.get(method);
    }

    public class DALRuntimeContext implements RuntimeContext {
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
            PropertyAccessor<Object> propertyAccessor = propertyAccessors.getData(data.getInstance());
            try {
                return propertyAccessor.getValueByData(data, property);
            } catch (InvalidPropertyException e) {
                return data.currying(property).orElseThrow(() -> e).resolve();
            } catch (InvocationException e) {
                throw e;
            } catch (Exception e) {
                throw new InvocationException(e);
            }
        }

        @SuppressWarnings("unchecked")
        public Iterable<Object> getList(Object instance) {
            return listAccessors.tryGetData(instance).map(l -> (Iterable<Object>) l.toIterable(instance))
                    .orElseGet(() -> arrayIterable(instance));
        }

        public int getListFirstIndex(Object instance) {
            return listAccessors.tryGetData(instance).map(listAccessor -> listAccessor.firstIndex(instance)).orElse(0);
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
                schemaBeanClass = create(Array.newInstance(schemaBeanClass.getType(), 0).getClass());
            return new Data(instance, this, SchemaType.create(schemaBeanClass));
        }

        public <T> DALRuntimeContext registerPropertyAccessor(T instance) {
            if (!Objects.equals(instance, null) && !propertyAccessors.containsType(instance))
                propertyAccessors.put(BeanClass.getClass(instance),
                        new JavaClassPropertyAccessor<>(BeanClass.createFrom(instance)));
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

        public Set<Method> methodToCurrying(Class<?> type, Object methodName) {
            return RuntimeContextBuilder.this.methodToCurrying(type, methodName);
        }

        public Function<MetaData, Object> fetchMetaFunction(DALNode property) {
            return metaProperties.computeIfAbsent(property.getRootSymbolName(), k -> {
                throw new RuntimeException(format("Meta property `%s` not found", property.getRootSymbolName()),
                        property.getPositionBegin());
            });
        }

        public TextBlockAttribute getAttribute(String name, int position) {
            return textAttributeMap.computeIfAbsent(name, k -> {
                throw new SyntaxException(format("Invalid text block attribute `%s`, all supported attributes are:\n%s",
                        k, textAttributeMap.entrySet().stream().map(e -> format("  %s:\n    %s",
                                e.getKey(), e.getValue().description())).collect(joining("\n"))), position);
            });
        }
    }
}
