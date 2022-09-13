package com.github.leeonky.dal.runtime.verifier;

import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.IllegalTypeException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.Schema;
import com.github.leeonky.dal.runtime.SchemaAssertionFailure;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.dal.type.SubType;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.PropertyReader;

import java.util.Set;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.util.stream.Collectors.toSet;

public class SchemaVerifier {
    private final Data object;
    private final DALRuntimeContext runtimeContext;
    private static final Compiler compiler = new Compiler();

    public SchemaVerifier(DALRuntimeContext runtimeContext, Data object) {
        this.runtimeContext = runtimeContext;
        this.object = object;
    }

    @SuppressWarnings("unchecked")
    private <T> BeanClass<T> getPolymorphicSchemaType(Class<?> superSchemaType) {
        Class<?> type = superSchemaType;
        SubType subType = superSchemaType.getAnnotation(SubType.class);
        if (subType != null) {
            Object value = object.getValue(compiler.toChainNodes(subType.property())).getInstance();
            type = Stream.of(subType.types())
                    .filter(t -> t.value().equals(value))
                    .map(SubType.Type::type)
                    .findFirst().orElseThrow(() -> new IllegalStateException(
                            format("Cannot guess sub type through property type value[%s]", value)));
        }
        return (BeanClass<T>) BeanClass.create(type);
    }

    public boolean verify(Class<?> clazz, Object schemaInstance, String subPrefix) {
        Set<String> propertyReaderNames = object.getFieldNames().stream().filter(String.class::isInstance)
                .map(Object::toString).collect(toSet());
        BeanClass<Object> schemaType = getPolymorphicSchemaType(clazz);
        Object schema = schemaInstance == null ? schemaType.newInstance() : schemaInstance;
        return (clazz.getAnnotation(Partial.class) != null ||
                noMoreUnexpectedField(schemaType, schemaType.getPropertyReaders().keySet(), propertyReaderNames))
                && allMandatoryPropertyShouldBeExist(schemaType, propertyReaderNames)
                && allPropertyValueShouldBeValid(subPrefix, schemaType, schema)
                && schemaVerificationShouldPass(schema);
    }

    private boolean schemaVerificationShouldPass(Object schema) {
        if (schema instanceof Schema) {
            try {
                ((Schema) schema).verify(object);
            } catch (SchemaAssertionFailure schemaAssertionFailure) {
                return errorLog(schemaAssertionFailure.getMessage());
            }
        }
        return true;
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
                    Data wrappedPropertyValue = object.getValue(propertyReader.getName());
                    return allowNullAndIsNull(propertyReader, wrappedPropertyValue)
                            || wrappedPropertyValue.createSchemaVerifier()
                            .verifySchemaInGenericType(subPrefix + "." + propertyReader.getName(),
                                    propertyReader.getType(), propertyReader.getValue(schemaInstance));
                });
    }

    private <T> boolean allowNullAndIsNull(PropertyReader<T> propertyReader, Data propertyValueWrapper) {
        return propertyReader.getAnnotation(AllowNull.class) != null && propertyValueWrapper.isNull();
    }

    private <T> boolean shouldNotContainsUnexpectedField(BeanClass<T> polymorphicBeanClass, Set<String> expectedFields, String f) {
        return expectedFields.contains(f) || errorLog("Unexpected field `%s` for schema %s[%s]", f,
                polymorphicBeanClass.getSimpleName(), polymorphicBeanClass.getName());
    }

    private <T> boolean shouldContainsField(Set<String> actualFields, BeanClass<T> polymorphicBeanClass, PropertyReader<T> propertyReader) {
        return actualFields.contains(propertyReader.getName())
                || errorLog("Expecting field `%s` to be in type %s[%s], but does not exist", propertyReader.getName(),
                polymorphicBeanClass.getSimpleName(), polymorphicBeanClass.getName());
    }

    static boolean errorLog(String format, Object... params) {
        throw new IllegalTypeException(String.format(format, params));
    }

    public boolean verifySchemaInGenericType(String subPrefix, BeanClass<?> type, Object schemaProperty) {
        return JavaValueSchema.createFieldSchema(subPrefix, type, schemaProperty, runtimeContext, object).verify(runtimeContext);
    }

    static IllegalStateException illegalStateException(String subPrefix) {
        return new IllegalStateException(format("%s should specify generic type", subPrefix));
    }

    static boolean shouldBeSameSize(String subPrefix, int actualSize, int expectSize) {
        return actualSize == expectSize
                || errorLog("Expecting field `%s` to be size [%d], but was size [%d]", subPrefix, expectSize, actualSize);
    }
}
