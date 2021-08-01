package com.github.leeonky.dal.util;

import com.github.leeonky.dal.IllegalFieldException;
import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
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
    private final DataObject object;
    private final RuntimeContext runtimeContext;

    public SchemaVerifier(RuntimeContext runtimeContext, DataObject object) {
        this.runtimeContext = runtimeContext;
        this.object = object;
    }

    @SuppressWarnings("unchecked")
    private <T> BeanClass<T> getPolymorphicSchemaType(Class<?> superSchemaType) {
        Class<?> type = superSchemaType;
        SubType subType = superSchemaType.getAnnotation(SubType.class);
        if (subType != null) {
            Object value = object.getPropertyValueBk(subType.property());
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
                    DataObject wrappedPropertyValue = object.getWrappedPropertyValue(propertyReader.getName());
                    return allowNullAndIsNull(propertyReader, wrappedPropertyValue)
                            || wrappedPropertyValue.createSchemaVerifier()
                            .verifySchemaInGenericType(subPrefix + "." + propertyReader.getName(),
                                    propertyReader.getType(), propertyReader.getValue(schemaInstance));
                });
    }

    private <T> boolean allowNullAndIsNull(PropertyReader<T> propertyReader, DataObject propertyValueWrapper) {
        return propertyReader.getAnnotation(AllowNull.class) != null && propertyValueWrapper.isNull();
    }

    private <T> boolean shouldNotContainsUnexpectedField(BeanClass<T> polymorphicBeanClass, Set<String> expectedFields, String f) {
        return expectedFields.contains(f)
                || errorLog("Unexpected field `%s` for type %s[%s]", f, polymorphicBeanClass.getSimpleName(), polymorphicBeanClass.getName());
    }

    private <T> boolean shouldContainsField(Set<String> actualFields, BeanClass<T> polymorphicBeanClass, PropertyReader<T> propertyReader) {
        return actualFields.contains(propertyReader.getName())
                || errorLog("Expected field `%s` for type %s[%s], but does not exist", propertyReader.getName(),
                polymorphicBeanClass.getSimpleName(), polymorphicBeanClass.getName());
    }

    private boolean errorLog(String format, Object... params) {
        System.err.printf(format + "%n", params);
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
        else if (Value.class.isAssignableFrom(fieldType))
            return verifyWrappedValue(subPrefix, (Value<Object>) schemaProperty, type);
        else
            return verifyType(subPrefix, schemaProperty, fieldType);
    }

    private boolean verifyWrappedValue(String subPrefix, Value<Object> schemaProperty, BeanClass<?> genericType) {
        BeanClass<?> type = genericType.getTypeArguments(0).orElse(null);
        if (schemaProperty != null)
            return verifyByValue(subPrefix, schemaProperty, type);
        if (type == null)
            throw illegalStateExcpetion(subPrefix);
        return verifyValueViaType(subPrefix, type.getType());
    }

    private boolean verifyValueViaType(String subPrefix, Class<?> rawType) {
        try {
            if (object.isNull())
                return errorLog("Can not convert null field `%s` to type [%s], use @AllowNull to verify nullable field",
                        subPrefix, rawType.getName());
            runtimeContext.getConverter().convert(rawType, object.getInstance());
            return true;
        } catch (Exception ignore) {
            return errorLog("Can not convert field `%s` (%s: %s) to type [%s]", subPrefix,
                    getClassName(object.getInstance()), object.getInstance(), rawType.getName());
        }
    }

    private boolean verifyByValue(String subPrefix, Value<Object> schemaProperty, BeanClass<?> type) {
        try {
            return schemaProperty.verify(schemaProperty.convertAs(runtimeContext, object.getInstance(), type))
                    || errorLog(schemaProperty.errorMessage(subPrefix, object.getInstance()));
        } catch (IllegalFieldException ignore) {
            throw illegalStateExcpetion(subPrefix);
        }
    }

    private IllegalStateException illegalStateExcpetion(String subPrefix) {
        return new IllegalStateException(format("%s should specify generic type", subPrefix));
    }

    private boolean verifyWrappedType(String subPrefix, Type<Object> schemaProperty, BeanClass<?> genericType) {
        if (schemaProperty != null)
            return schemaProperty.verify(object.getInstance())
                    || errorLog(schemaProperty.errorMessage(subPrefix, object.getInstance()));
        Class<?> rawType = genericType.getTypeArguments(0)
                .orElseThrow(() -> illegalStateExcpetion(subPrefix)).getType();
        return rawType.isInstance(object.getInstance())
                || errorLog("Expected field `%s` is type [%s], but was [%s]", subPrefix,
                rawType.getName(), getClassName(object.getInstance()));
    }

    private boolean verifyType(String subPrefix, Object schemaProperty, Class<?> fieldType) {
        if (schemaProperty != null)
            return Objects.equals(schemaProperty, object.getInstance())
                    || errorLog("Expected field `%s` equal to %s[%s], but was %s[%s]", subPrefix,
                    getClassName(schemaProperty), schemaProperty, getClassName(object.getInstance()), object.getInstance());
        return fieldType.isInstance(object.getInstance())
                || errorLog("Expected field `%s` is type [%s], but was [%s]", subPrefix,
                fieldType.getName(), getClassName(object.getInstance()));
    }

    private boolean verifyCollection(String subPrefix, BeanClass<?> elementType, Object schemaProperties) {
        List<DataObject> dataObjectList = stream(object.getWrappedList().spliterator(), false)
                .collect(toList());
        if (schemaProperties == null)
            return range(0, dataObjectList.size())
                    .allMatch(i -> dataObjectList.get(i).createSchemaVerifier().verifySchemaInGenericType(
                            format("%s[%d]", subPrefix, i), elementType, null));
        else {
            List<Object> schemaPropertyList = arrayCollectionToStream(schemaProperties).collect(toList());
            return shouldBeSameSize(subPrefix, dataObjectList, schemaPropertyList)
                    && range(0, dataObjectList.size())
                    .allMatch(i -> dataObjectList.get(i).createSchemaVerifier().verifySchemaInGenericType(
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
                || errorLog("Expected field `%s` should be size [%d], but was size [%d]", subPrefix,
                schemaPropertyList.size(), wrappedObjectList.size());
    }

    private boolean verifyFormatterValue(String subPrefix, Formatter<Object, Object> formatter) {
        return formatter.isValid(object.getInstance())
                || errorLog("Expected field `%s` should be in `%s`, but was [%s]", subPrefix,
                formatter.getFormatterName(), object.getInstance());
    }
}
