package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Formatters;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.Converter;
import com.github.leeonky.util.NoSuchAccessorException;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Stream.of;

public class RuntimeContextBuilder {
    private final ClassKeyMap<PropertyAccessor<Object>> propertyAccessors = new ClassKeyMap<>();
    private final ClassKeyMap<ListAccessor<Object>> listAccessors = new ClassKeyMap<>();
    private final Map<String, ConstructorViaSchema> constructors = new LinkedHashMap<>();
    private final Map<String, BeanClass<?>> schemas = new HashMap<>();
    private final Set<Class<?>> staticMethodExtensionClasses = new HashSet<>();
    private Converter converter = Converter.createDefault();

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
                .registerSchema("List", DataObject::isList)
                .registerListAccessor(Iterable.class, iterable -> iterable)
                .registerListAccessor(Stream.class, stream -> stream::iterator)
                .registerPropertyAccessor(Map.class, new PropertyAccessor<Map<String, ?>>() {
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
                });
    }

    public RuntimeContext build(Object inputValue) {
        return new RuntimeContext(inputValue);
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
        return registerSchema(name, dataObject -> dataObject.createSchemaVerifier()
                .verify(schema, null, ""));
    }

    public RuntimeContextBuilder registerSchema(String name, Function<DataObject, Boolean> predicate) {
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
        staticMethodExtensionClasses.add(staticMethodExtensionClass);
        return this;
    }

    public class RuntimeContext {
        private final LinkedList<DataObject> thisStack = new LinkedList<>();
        private final Set<Class<?>> schemaSet;
        private boolean listMapping = false;
        private final List<Method> extensionMethods = staticMethodExtensionClasses.stream().flatMap(c -> of(c.getMethods()))
                .filter(RuntimeContextBuilder.this::maybeExtensionMethods)
                .collect(toList());

        public RuntimeContext(Object inputValue) {
            schemaSet = schemas.values().stream().map(BeanClass::getType).collect(Collectors.toSet());
            thisStack.push(wrap(inputValue));
        }

        public DataObject getInputValue() {
            return thisStack.getFirst();
        }

        public <T> T newThisScope(DataObject dataObject, Supplier<T> supplier) {
            try {
                thisStack.push(dataObject);
                return supplier.get();
            } finally {
                thisStack.pop();
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

        public Object getPropertyValue(Object instance, String name) {
            return propertyAccessors.getData(instance).getValue(instance, name);
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

        public DataObject wrap(Object instance) {
            return new DataObject(instance, this, SchemaType.createRoot());
        }

        public DataObject wrap(Object instance, String schema, boolean isList) {
            BeanClass<?> schemaBeanClass = schemas.get(schema);
            if (isList)
                schemaBeanClass = BeanClass.create(Array.newInstance(schemaBeanClass.getType(), 0).getClass());
            return new DataObject(instance, this, SchemaType.create(schemaBeanClass));
        }

        public RuntimeContext registerPropertyAccessor(Object instance) {
            if (!Objects.equals(instance, null) && !propertyAccessors.containsType(instance)) {
                propertyAccessors.put(BeanClass.getClass(instance), new PropertyAccessor<Object>() {
                    private final BeanClass<Object> beanClass = BeanClass.createFrom(instance);

                    @Override
                    public Object getValue(Object instance, String name) {
                        try {
                            return beanClass.getPropertyValue(instance, name);
                        } catch (NoSuchAccessorException ignore) {
                            try {
                                return beanClass.getType().getMethod(name).invoke(instance);
                            } catch (IllegalAccessException | NoSuchMethodException e) {
                                return invokeExtensionMethod(instance, name);
                            } catch (Exception e) {
                                throw new IllegalStateException(e);
                            }
                        }
                    }

                    @Override
                    public Set<String> getPropertyNames(Object instance) {
                        return beanClass.getPropertyReaders().keySet();
                    }

                    @Override
                    public boolean isNull(Object instance) {
                        return Objects.equals(instance, null);
                    }
                });
            }
            return this;
        }

        private Object invokeExtensionMethod(Object instance, String name) {
//            TODO more than one base class instance
//            TODO no method
            return FunctionUtil.oneOf(() -> findMethodWithSameType(instance, name),
                    () -> findMethodWithFromBaseType(instance, name)).map(method -> {
                try {
                    return method.invoke(null, instance);
                } catch (Exception e) {
                    throw new IllegalStateException(e);
                }
            }).orElse(null);
        }

        private Optional<Method> findMethodWithFromBaseType(Object instance, String name) {
            return extensionMethods.stream().filter(method -> methodWithBaseType(instance, name, method)).findFirst();
        }

        private Optional<Method> findMethodWithSameType(Object instance, String name) {
            return extensionMethods.stream().filter(method -> methodWithSameType(instance, name, method)).findFirst();
        }

        private boolean methodWithBaseType(Object instance, String name, Method method) {
            return method.getName().equals(name)
                    && method.getParameterTypes()[0].equals(instance.getClass());
        }

        private boolean methodWithSameType(Object instance, String name, Method method) {
            return method.getName().equals(name)
                    && method.getParameterTypes()[0].isAssignableFrom(instance.getClass());
        }

        public void beginListMapping() {
            listMapping = true;
        }

        public boolean isListMapping() {
            return listMapping;
        }

        public void endListMapping() {
            listMapping = false;
        }
    }

    private boolean maybeExtensionMethods(Method method) {
        return method.getParameterCount() == 1
                && Modifier.isStatic(method.getModifiers())
                && Modifier.isPublic(method.getModifiers());
    }
}
