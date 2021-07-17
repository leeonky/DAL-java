package com.github.leeonky.dal.util;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.dal.type.SubType;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.PropertyReader;

import java.util.*;
import java.util.stream.Stream;

import static com.github.leeonky.util.BeanClass.arrayCollectionToStream;
import static com.github.leeonky.util.BeanClass.getClassName;
import static java.lang.String.format;
import static java.util.stream.Collectors.toList;
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
                    .findFirst().orElseThrow(() -> new IllegalStateException(
                            format("Cannot guess sub type through property type value[%s]", value)));
        }
        return (BeanClass<T>) BeanClass.create(type);
    }

    public boolean verify(Class<?> clazz, Object schemaInstance, String subPrefix) {
        Set<String> propertyReaderNames = object.getPropertyReaderNames();
        BeanClass<Object> schema = getPolymorphicSchemaType(clazz);
        return (clazz.getAnnotation(Partial.class) != null ||
                noMoreUnexpectedField(schema, schema.getPropertyReaders().keySet(), propertyReaderNames))
                && allMandatoryPropertyShouldBeExist(schema, propertyReaderNames)
                && allPropertyValueShouldBeValid(subPrefix, schema,
                schemaInstance == null ? schema.newInstance() : schemaInstance);
    }

    private <T> boolean noMoreUnexpectedField(BeanClass<T> polymorphicBeanClass, Set<String> expectedFields, Set<String> actualFields) {
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
                            || wrappedPropertyValue.createSchemaVerifier()
                            .verifySchemaInGenericType(subPrefix + "." + propertyReader.getName(),
                                    propertyReader.getType(), propertyReader.getValue(schemaInstance));
                });
    }

    private <T> boolean allowNullAndIsNull(PropertyReader<T> propertyReader, WrappedObject propertyValueWrapper) {
        return propertyReader.getAnnotation(AllowNull.class) != null && propertyValueWrapper.isNull();
    }

    private <T> boolean shouldNotContainsUnexpectedField(BeanClass<T> polymorphicBeanClass, Set<String> expectedFields, String f) {
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
    private boolean verifySchemaInGenericType(String subPrefix, BeanClass<?> type, Object schemaProperty) {
        Class<?> fieldType = type.getType();
        if (Formatter.class.isAssignableFrom(fieldType)) {
            return verifyFormatterValue(subPrefix, getOrCreateFormatter(schemaProperty, type));
        } else if (runtimeContext.isRegistered(fieldType))
            return object.createSchemaVerifier().verify(fieldType, schemaProperty, subPrefix);
        else if (type.isCollection())
            return verifyCollection(subPrefix, type.getElementType(), schemaProperty);
        else if (Map.class.isAssignableFrom(fieldType))
            return verifyMap(subPrefix, type, (Map<?, Object>) schemaProperty);
        else if (Type.class.isAssignableFrom(fieldType))
            return verifyWrappedType(subPrefix, (Type<Object>) schemaProperty, type);
        else
            return verifyType(subPrefix, schemaProperty, fieldType);
    }

    private boolean verifyWrappedType(String subPrefix, Type<Object> schemaProperty, BeanClass<?> genericType) {
        if (schemaProperty != null)
            return schemaProperty.verify(object.getInstance())
                    || errorLog("Field `%s` is invalid\n", subPrefix);
        Class<?> rawType = genericType.getTypeArguments(0)
                .orElseThrow(() -> new IllegalStateException(format("%s should specify generic type", subPrefix))).getType();
        return rawType.isInstance(object.getInstance())
                || errorLog("Expected field `%s` for type [%s], but was [%s]\n", subPrefix,
                rawType.getName(), getClassName(object.getInstance()));
    }

    private boolean verifyType(String subPrefix, Object schemaProperty, Class<?> fieldType) {
        if (schemaProperty != null)
            return Objects.equals(schemaProperty, object.getInstance())
                    || errorLog("Expected field `%s` equal to %s[%s], but was %s[%s]\n", subPrefix,
                    getClassName(schemaProperty), schemaProperty, getClassName(object.getInstance()), object.getInstance());
        return fieldType.isInstance(object.getInstance())
                || errorLog("Expected field `%s` for type [%s], but was [%s]\n", subPrefix,
                fieldType.getName(), getClassName(object.getInstance()));
    }

    private boolean verifyCollection(String subPrefix, BeanClass<?> elementType, Object schemaProperties) {
        List<WrappedObject> wrappedObjectList = stream(object.getWrappedList().spliterator(), false)
                .collect(toList());
        if (schemaProperties == null)
            return range(0, wrappedObjectList.size())
                    .allMatch(i -> wrappedObjectList.get(i).createSchemaVerifier().verifySchemaInGenericType(
                            format("%s[%d]", subPrefix, i), elementType, null));
        else {
            List<Object> schemaPropertyList = arrayCollectionToStream(schemaProperties).collect(toList());
            return shouldBeSameSize(subPrefix, wrappedObjectList, schemaPropertyList)
                    && range(0, wrappedObjectList.size())
                    .allMatch(i -> wrappedObjectList.get(i).createSchemaVerifier().verifySchemaInGenericType(
                            format("%s[%d]", subPrefix, i), elementType, schemaPropertyList.get(i)));
        }
    }

    @SuppressWarnings("unchecked")
    private Formatter<Object, Object> getOrCreateFormatter(Object schemaProperty, BeanClass<?> genericType) {
        if (schemaProperty != null)
            return (Formatter<Object, Object>) schemaProperty;
        Class<Object> fieldType = (Class<Object>) genericType.getType();
        return (Formatter<Object, Object>) genericType.getTypeArguments(0)
                .map(t -> BeanClass.newInstance(fieldType, t.getType()))
                .orElseGet(() -> BeanClass.newInstance(fieldType));
    }

    private boolean verifyMap(String subPrefix, BeanClass<?> genericType, Map<?, Object> schemaProperty) {
        BeanClass<?> subGenericType = genericType.getTypeArguments(1).orElseThrow(() ->
                new IllegalArgumentException(format("`%s` should be generic type", subPrefix)));
        if (schemaProperty == null)
            return object.getPropertyReaderNames().stream()
                    .allMatch(key -> object.getWrappedPropertyValue(key).createSchemaVerifier()
                            .verifySchemaInGenericType(subPrefix + "." + key, subGenericType, null));
        return shouldBeSameSize(subPrefix, object.getPropertyReaderNames(), schemaProperty.values())
                && object.getPropertyReaderNames().stream()
                .allMatch(key -> object.getWrappedPropertyValue(key).createSchemaVerifier()
                        .verifySchemaInGenericType(subPrefix + "." + key, subGenericType, schemaProperty.get(key)));
    }

    private boolean shouldBeSameSize(String subPrefix, Collection<?> wrappedObjectList, Collection<?> schemaPropertyList) {
        return wrappedObjectList.size() == schemaPropertyList.size()
                || errorLog("Expected field `%s` should be size [%d], but was size [%d]\n", subPrefix,
                schemaPropertyList.size(), wrappedObjectList.size());
    }

    private boolean verifyFormatterValue(String subPrefix, Formatter<Object, Object> formatter) {
        return formatter.isValid(object.getInstance())
                || errorLog("Expected field `%s` should be in `%s`, but was [%s]\n", subPrefix,
                formatter.getFormatterName(), object.getInstance());
    }
}
