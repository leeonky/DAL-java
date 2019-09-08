package com.github.leeonky.dal.util;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.SubType;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.GenericType;
import com.github.leeonky.util.PropertyReader;

import java.lang.reflect.Array;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.IntStream.range;
import static java.util.stream.StreamSupport.stream;

public class WrappedObject {
    private final Object instance;
    private final BeanClass<Object> beanClass;
    private final RuntimeContext runtimeContext;

    @SuppressWarnings("unchecked")
    public WrappedObject(Object instance, RuntimeContext context) {
        this.instance = instance;
        beanClass = instance == null ? null : (BeanClass<Object>) BeanClass.create(instance.getClass());
        runtimeContext = context;
    }

    public boolean isList() {
        return instance != null && (runtimeContext.isRegisteredList(instance) || instance instanceof Iterable || instance.getClass().isArray());
    }

    @SuppressWarnings("unchecked")
    public Set<String> getPropertyReaderNames() {
        return runtimeContext.findPropertyReaderNames(instance)
                .orElseGet(() -> {
                    if (instance instanceof Map)
                        return ((Map) instance).keySet();
                    return beanClass.getPropertyReaders().keySet();
                });
    }

    public Object getPropertyValue(String name) {
        if (name.contains(".")) {
            String[] split = name.split("\\.", 2);
            return getWrappedPropertyValue(split[0]).getPropertyValue(split[1]);
        }
        return instance instanceof Map ?
                ((Map) instance).get(name)
                : getPropertyFromType(name);
    }

    public WrappedObject getWrappedPropertyValue(String name) {
        return runtimeContext.wrap(getPropertyValue(name));
    }

    public int getListSize() {
        int size = 0;
        for (Object ignore : getList())
            size++;
        return size;
    }

    public Iterable getList() {
        return runtimeContext.gitList(instance)
                .orElseGet(() -> {
                    if (instance instanceof Iterable)
                        return (Iterable) instance;
                    return () -> new Iterator() {
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
                });
    }

    public Iterable<WrappedObject> getWrappedList() {
        List<WrappedObject> result = new ArrayList<>();
        for (Object object : getList())
            result.add(runtimeContext.wrap(object));
        return result;
    }

    private Object getPropertyFromType(String name) {
        return runtimeContext.getPropertyValue(instance, name)
                .orElseGet(() -> beanClass.getPropertyValue(instance, name));
    }

    public Boolean isNull() {
        return runtimeContext.isNull(instance);
    }

    private BeanClass getPolymorphicSchemaType(Class<?> superSchemaType) {
        Class<?> type = superSchemaType;
        SubType subType = superSchemaType.getAnnotation(SubType.class);
        if (subType != null) {
            Object value = getPropertyValue(subType.property());
            type = Stream.of(subType.types())
                    .filter(t -> t.value().equals(value))
                    .map(SubType.Type::type)
                    .findFirst().orElseThrow(() -> new IllegalStateException(String.format("Cannot guess sub type through property type value[%s]", value)));
        }
        return BeanClass.create(type);
    }

    @SuppressWarnings("unchecked")
    public boolean verifySchema(Class<?> schemaType, String subPrefix) {
        BeanClass<Object> polymorphicBeanClass = getPolymorphicSchemaType(schemaType);
        Set<String> propertyReaderNames = getPropertyReaderNames();
        Object schemaInstance = polymorphicBeanClass.newInstance();

        return noMoreUnexpectedField(polymorphicBeanClass, polymorphicBeanClass.getPropertyReaders().keySet(), propertyReaderNames)
                && allMandatoryPropertyShouldBeExist(polymorphicBeanClass, propertyReaderNames)
                && allPropertyValueShouldBeValid(subPrefix, polymorphicBeanClass, schemaInstance);
    }

    private boolean allMandatoryPropertyShouldBeExist(BeanClass<?> polymorphicBeanClass, Set<String> actualFields) {
        return polymorphicBeanClass.getPropertyReaders().values().stream()
                .filter(isAllowNull().negate())
                .filter(propertyReader -> !actualFields.contains(propertyReader.getName()))
                .peek(propertyReader -> System.err.printf("Expected field `%s` for type %s[%s], but does not exist\n",
                        propertyReader.getName(), polymorphicBeanClass.getSimpleName(), polymorphicBeanClass.getName()))
                .count() == 0;
    }

    private Predicate<PropertyReader<?>> isAllowNull() {
        return propertyReader -> propertyReader.getAnnotation(AllowNull.class) != null;
    }

    private boolean noMoreUnexpectedField(BeanClass polymorphicBeanClass, Set<String> expectedFields, Set<String> actualFields) {
        return actualFields.stream()
                .filter(f -> !expectedFields.contains(f))
                .peek(f -> System.err.printf("Unexpected field `%s` for type %s[%s]\n", f, polymorphicBeanClass.getSimpleName(), polymorphicBeanClass.getName()))
                .count() == 0;
    }

    private <T> boolean allPropertyValueShouldBeValid(String subPrefix, BeanClass<T> polymorphicBeanClass, T schemaInstance) {
        return polymorphicBeanClass.getPropertyReaders().values().stream()
                .noneMatch(propertyReader -> {
                    WrappedObject propertyValueWrapper = getWrappedPropertyValue(propertyReader.getName());
                    if (isAllowNull().test(propertyReader) && propertyValueWrapper.isNull())
                        return false;

                    return !propertyValueWrapper.verifySchemaInGenericType(subPrefix + "." + propertyReader.getName(),
                            propertyReader.getGenericType(), propertyReader.getValue(schemaInstance));
                });
    }

    @SuppressWarnings("unchecked")
    private <T> boolean verifySchemaInGenericType(String subPrefix, GenericType genericType, Object schemaProperty) {
        Class<?> fieldType = genericType.getRawType();
        if (Formatter.class.isAssignableFrom(fieldType)) {
            Optional<GenericType> genericTypeParameter = genericType.getGenericTypeParameter(0);
            Object formatter = schemaProperty;
            if (formatter == null)
                formatter = genericTypeParameter.isPresent() ? BeanClass.newInstance(fieldType, genericTypeParameter.get().getRawType())
                        : BeanClass.newInstance(fieldType);
            return verifyFormatterValue(subPrefix, (Formatter<Object, Object>) formatter);
        } else if (runtimeContext.isRegistered(fieldType))
            return verifySchema(fieldType, subPrefix);
        else if (Iterable.class.isAssignableFrom(fieldType))
            return verifyList(subPrefix, genericType, (Iterable) schemaProperty);
        else if (Map.class.isAssignableFrom(fieldType))
            return verifyMap(subPrefix, genericType);
        return true;
    }

    private boolean verifyFormatterValue(String subPrefix, Formatter<Object, Object> formatter) {
        return formatter.isValidValue(instance)
                || errorLog("Expected field `%s` should be in `%s`, but was [%s]\n", subPrefix, formatter.getFormatterName(), instance);
    }

    private boolean errorLog(String format, Object... params) {
        System.err.printf(format, params);
        return false;
    }

    private boolean verifyList(String subPrefix, GenericType genericType, Iterable<Object> schemaProperties) {
        GenericType subGenericType = genericType.getGenericTypeParameter(0).orElseThrow(() ->
                new IllegalArgumentException(subPrefix + " should be generic type"));
        List<WrappedObject> wrappedObjectList = stream(getWrappedList().spliterator(), false)
                .collect(Collectors.toList());

        if (schemaProperties == null)
            return range(0, wrappedObjectList.size())
                    .allMatch(i -> wrappedObjectList.get(i).
                            verifySchemaInGenericType(String.format("%s[%d]", subPrefix, i), subGenericType, null));
        else {
            List<Object> schemaPropertyList = stream(schemaProperties.spliterator(), false)
                    .collect(Collectors.toList());
            return shouldBeSameSize(subPrefix, wrappedObjectList, schemaPropertyList)
                    && range(0, wrappedObjectList.size())
                    .allMatch(i -> wrappedObjectList.get(i).
                            verifySchemaInGenericType(String.format("%s[%d]", subPrefix, i), subGenericType, schemaPropertyList.get(i)));
        }
    }

    private boolean shouldBeSameSize(String subPrefix, List<WrappedObject> wrappedObjectList, List<Object> schemaPropertyList) {
        return wrappedObjectList.size() == schemaPropertyList.size()
                || errorLog("Expected field `%s` should be size [%d], but was size [%d]\n", subPrefix, schemaPropertyList.size(), wrappedObjectList.size());
    }

    private boolean verifyMap(String subPrefix, GenericType genericType) {
        GenericType subGenericType = genericType.getGenericTypeParameter(1).orElseThrow(() ->
                new IllegalArgumentException(String.format("`%s` should be generic type", subPrefix)));
        return getPropertyReaderNames().stream()
                .allMatch(key -> getWrappedPropertyValue(key).verifySchemaInGenericType(subPrefix + "." + key, subGenericType, null));
    }
}
