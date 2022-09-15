package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.dal.type.SubType;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.PropertyReader;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.util.stream.Collectors.toSet;

public class SchemaExpectation extends CompositeExpectation {
    private final boolean partial;
    private static final Compiler compiler = new Compiler();

    private SchemaExpectation(String property, BeanClass<?> type, BeanClass<Object> newType, Object expect) {
        super(newType, property, expect == null ? newType.newInstance() : expect);
        partial = type.getType().getAnnotation(Partial.class) != null;
    }

    public SchemaExpectation(String property, BeanClass<?> type, Data actual, Object expect) {
        this(property, type, (BeanClass<Object>) getPolymorphicSchemaType(type.getType(), actual), expect);
    }

    public SchemaExpectation(String property, BeanClass<?> type, Data actual) {
        this(property, type, actual, null);
    }

    private static BeanClass<?> getPolymorphicSchemaType(Class<?> schemaType, Data actual) {
        Class<?> type = schemaType;
        SubType subType = schemaType.getAnnotation(SubType.class);
        if (subType != null) {
            Object subTypeProperty = actual.getValue(compiler.toChainNodes(subType.property())).getInstance();
            type = Stream.of(subType.types()).filter(t -> t.value().equals(subTypeProperty)).map(SubType.Type::type)
                    .findFirst().orElseThrow(() -> new IllegalStateException(
                            format("Cannot guess sub type through property type value[%s]", subTypeProperty)));
        }
        return BeanClass.create(type);
    }

    public static boolean errorLog(String format, Object... params) {
        throw new IllegalTypeException(String.format(format, params));
    }

    @Override
    public boolean verify(Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        Set<String> actualFields = actual.getFieldNames().stream().filter(String.class::isInstance)
                .map(Object::toString).collect(toSet());
        return (partial || noMoreUnexpectedField(actualFields))
                && allMandatoryPropertyShouldBeExist(actualFields)
                && allPropertyValueShouldBeValid(actual, runtimeContext)
                && schemaVerificationShouldPass(expect, actual);
    }


    private boolean allMandatoryPropertyShouldBeExist(Set<String> actualFields) {
        return type.getPropertyReaders().values().stream()
                .filter(field -> field.getAnnotation(AllowNull.class) == null)
                .allMatch(field -> actualFields.contains(field.getName())
                        || errorLog("Expecting field `%s` to be in type %s[%s], but does not exist", field.getName(),
                        type.getSimpleName(), type.getName()));
    }


    private boolean allPropertyValueShouldBeValid(Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        return type.getPropertyReaders().values().stream().allMatch(propertyReader -> {
            Data subActual = actual.getValue(propertyReader.getName());
            return allowNullAndIsNull(propertyReader, subActual)
                    || subExpectation(propertyReader).verify(subActual, runtimeContext);
        });
    }

    private boolean allowNullAndIsNull(PropertyReader<?> propertyReader, Data propertyValueWrapper) {
        return propertyReader.getAnnotation(AllowNull.class) != null && propertyValueWrapper.isNull();
    }

    private boolean noMoreUnexpectedField(Set<String> actualFields) {
        Set<String> expectFields = new LinkedHashSet<String>(actualFields) {{
            removeAll(type.getPropertyReaders().keySet());
        }};
        return expectFields.isEmpty() || errorLog("Unexpected field %s for schema %s[%s]",
                expectFields.stream().collect(Collectors.joining("`, `", "`", "`")),
                type.getSimpleName(), type.getName());
    }

    private boolean schemaVerificationShouldPass(Object schema, Data actual) {
        if (schema instanceof Schema) {
            try {
                ((Schema) schema).verify(actual);
            } catch (SchemaAssertionFailure schemaAssertionFailure) {
                errorLog(schemaAssertionFailure.getMessage());
            }
        }
        return true;
    }
}
