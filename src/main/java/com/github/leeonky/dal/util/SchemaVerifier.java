package com.github.leeonky.dal.util;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.SubType;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.GenericType;
import com.github.leeonky.util.PropertyReader;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.IntStream.range;
import static java.util.stream.StreamSupport.stream;

public class SchemaVerifier {
    private final WrappedObject object;
    private final RuntimeContext runtimeContext;

    public SchemaVerifier(RuntimeContext runtimeContext, WrappedObject object) {
        this.runtimeContext = runtimeContext;
        this.object = object;
    }

    @SuppressWarnings("unchecked")
    private <T> BeanClass<T> getPolymorphicSchemaType(Class<?> superSchemaType) {
        Class<?> type = superSchemaType;
        SubType subType = superSchemaType.getAnnotation(SubType.class);
        if (subType != null) {
            Object value = object.getPropertyValue(subType.property());
            type = Stream.of(subType.types())
                    .filter(t -> t.value().equals(value))
                    .map(SubType.Type::type)
                    .findFirst().orElseThrow(() -> new IllegalStateException(String.format("Cannot guess sub type through property type value[%s]", value)));
        }
        return (BeanClass<T>) BeanClass.create(type);
    }

    public boolean verify(Class<?> clazz, Object schemaInstance, String subPrefix) {
        Set<String> propertyReaderNames = object.getPropertyReaderNames();
        BeanClass<Object> schema = getPolymorphicSchemaType(clazz);
        return noMoreUnexpectedField(schema, schema.getPropertyReaders().keySet(), propertyReaderNames)
                && allMandatoryPropertyShouldBeExist(schema, propertyReaderNames)
                && allPropertyValueShouldBeValid(subPrefix, schema, schemaInstance == null ? schema.newInstance() : schemaInstance);
    }

    private boolean noMoreUnexpectedField(BeanClass polymorphicBeanClass, Set<String> expectedFields, Set<String> actualFields) {
        return actualFields.stream()
                .allMatch(f -> shouldNotContainsUnexpectedField(polymorphicBeanClass, expectedFields, f));
    }

    private <T> boolean allMandatoryPropertyShouldBeExist(BeanClass<T> polymorphicBeanClass, Set<String> actualFields) {
        return polymorphicBeanClass.getPropertyReaders().values().stream()
                .filter(propertyReader -> propertyReader.getAnnotation(AllowNull.class) == null)
                .allMatch(propertyReader -> shouldContainsField(actualFields, polymorphicBeanClass, propertyReader));
    }

    private <T> boolean allPropertyValueShouldBeValid(String subPrefix, BeanClass<T> polymorphicBeanClass, T schemaInstance) {
        return polymorphicBeanClass.getPropertyReaders().values().stream()
                .allMatch(propertyReader -> {
                    WrappedObject wrappedPropertyValue = object.getWrappedPropertyValue(propertyReader.getName());
                    return allowNullAndIsNull(propertyReader, wrappedPropertyValue)
                            || wrappedPropertyValue.createSchemaVerifier().verifySchemaInGenericType(subPrefix + "." + propertyReader.getName(),
                            propertyReader.getGenericType(), propertyReader.getValue(schemaInstance));
                });
    }

    private <T> boolean allowNullAndIsNull(PropertyReader<T> propertyReader, WrappedObject propertyValueWrapper) {
        return propertyReader.getAnnotation(AllowNull.class) != null && propertyValueWrapper.isNull();
    }

    private boolean shouldNotContainsUnexpectedField(BeanClass polymorphicBeanClass, Set<String> expectedFields, String f) {
        return expectedFields.contains(f)
                || errorLog("Unexpected field `%s` for type %s[%s]\n", f, polymorphicBeanClass.getSimpleName(), polymorphicBeanClass.getName());
    }

    private <T> boolean shouldContainsField(Set<String> actualFields, BeanClass<T> polymorphicBeanClass, PropertyReader<T> propertyReader) {
        return actualFields.contains(propertyReader.getName())
                || errorLog("Expected field `%s` for type %s[%s], but does not exist\n", propertyReader.getName(),
                polymorphicBeanClass.getSimpleName(), polymorphicBeanClass.getName());
    }

    private boolean errorLog(String format, Object... params) {
        System.err.printf(format, params);
        return false;
    }

    @SuppressWarnings("unchecked")
    private <T> boolean verifySchemaInGenericType(String subPrefix, GenericType genericType, Object schemaProperty) {
        Class<?> fieldType = genericType.getRawType();
        if (Formatter.class.isAssignableFrom(fieldType)) {
            return verifyFormatterValue(subPrefix, getOrCreateFormatter(schemaProperty, genericType));
        } else if (runtimeContext.isRegistered(fieldType))
            return object.createSchemaVerifier().verify(fieldType, schemaProperty, subPrefix);
        else if (Iterable.class.isAssignableFrom(fieldType))
            return verifyList(subPrefix, genericType, (Iterable) schemaProperty);
        else if (Map.class.isAssignableFrom(fieldType))
            return verifyMap(subPrefix, genericType, (Map) schemaProperty);
        return true;
    }

    @SuppressWarnings("unchecked")
    private Formatter<Object, Object> getOrCreateFormatter(Object schemaProperty, GenericType genericType) {
        if (schemaProperty != null)
            return (Formatter<Object, Object>) schemaProperty;
        Class<Object> fieldType = (Class<Object>) genericType.getRawType();
        return (Formatter<Object, Object>) genericType.getGenericTypeParameter(0)
                .map(t -> BeanClass.newInstance(fieldType, t.getRawType()))
                .orElseGet(() -> BeanClass.newInstance(fieldType));
    }

    private boolean verifyList(String subPrefix, GenericType genericType, Iterable<Object> schemaProperties) {
        GenericType subGenericType = genericType.getGenericTypeParameter(0).orElseThrow(() ->
                new IllegalArgumentException(subPrefix + " should be generic type"));
        List<WrappedObject> wrappedObjectList = stream(object.getWrappedList().spliterator(), false)
                .collect(Collectors.toList());

        if (schemaProperties == null)
            return range(0, wrappedObjectList.size())
                    .allMatch(i -> wrappedObjectList.get(i).createSchemaVerifier().verifySchemaInGenericType(String.format("%s[%d]", subPrefix, i), subGenericType, null));
        else {
            List<Object> schemaPropertyList = stream(schemaProperties.spliterator(), false)
                    .collect(Collectors.toList());
            return shouldBeSameSize(subPrefix, wrappedObjectList, schemaPropertyList)
                    && range(0, wrappedObjectList.size())
                    .allMatch(i -> wrappedObjectList.get(i).createSchemaVerifier().verifySchemaInGenericType(String.format("%s[%d]", subPrefix, i), subGenericType, schemaPropertyList.get(i)));
        }
    }

    private boolean verifyMap(String subPrefix, GenericType genericType, Map<?, Object> schemaProperty) {
        GenericType subGenericType = genericType.getGenericTypeParameter(1).orElseThrow(() ->
                new IllegalArgumentException(String.format("`%s` should be generic type", subPrefix)));
        if (schemaProperty == null)
            return object.getPropertyReaderNames().stream()
                    .allMatch(key -> object.getWrappedPropertyValue(key).createSchemaVerifier().verifySchemaInGenericType(subPrefix + "." + key, subGenericType, null));
        return shouldBeSameSize(subPrefix, object.getPropertyReaderNames(), schemaProperty.values())
                && object.getPropertyReaderNames().stream()
                .allMatch(key -> object.getWrappedPropertyValue(key).createSchemaVerifier().verifySchemaInGenericType(subPrefix + "." + key, subGenericType, schemaProperty.get(key)));
    }

    private boolean shouldBeSameSize(String subPrefix, Collection<?> wrappedObjectList, Collection<?> schemaPropertyList) {
        return wrappedObjectList.size() == schemaPropertyList.size()
                || errorLog("Expected field `%s` should be size [%d], but was size [%d]\n", subPrefix, schemaPropertyList.size(), wrappedObjectList.size());
    }

    private boolean verifyFormatterValue(String subPrefix, Formatter<Object, Object> formatter) {
        return formatter.isValid(object.getInstance())
                || errorLog("Expected field `%s` should be in `%s`, but was [%s]\n", subPrefix, formatter.getFormatterName(), object.getInstance());
    }
}
