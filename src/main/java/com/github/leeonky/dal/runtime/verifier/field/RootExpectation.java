package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.dal.runtime.verifier.SchemaVerifier;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.dal.type.SubType;
import com.github.leeonky.util.BeanClass;

import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.github.leeonky.dal.format.Formatter.createFormatter;
import static com.github.leeonky.dal.runtime.verifier.SchemaVerifier.errorLog;
import static com.github.leeonky.dal.runtime.verifier.SchemaVerifier.shouldNotContainsUnexpectedField;
import static com.github.leeonky.dal.runtime.verifier.field.Factory.createFieldSchema;
import static com.github.leeonky.dal.runtime.verifier.field.Factory.illegalStateException;
import static com.github.leeonky.util.BeanClass.getClassName;
import static java.lang.String.format;
import static java.util.stream.Collectors.toSet;

public class RootExpectation<T> {
    protected final Object property;
    protected final T expect;
    protected final BeanClass<?> type;

    public RootExpectation(BeanClass<?> type, Object property, T expect) {
        this.property = property;
        this.expect = expect;
        this.type = type;
    }

    public Object getProperty() {
        return property;
    }

    public T getExpect() {
        return expect;
    }

    public BeanClass<?> getType() {
        return type;
    }

    protected String inspectExpectType() {
        return format("type [%s]", type.getName());
    }

    protected String fieldStr() {
        return "" + property;
    }

    public Expectation structureExpectation() {
        return (actual, runtimeContext) -> type.getType().isInstance(actual.getInstance())
                || errorLog(format("Expecting field `%s` to be %s, but was [%s]", fieldStr(), inspectExpectType(),
                getClassName(actual.getInstance())));
    }

    public Expectation contentExpectation() {
        return (actual, runtimeContext) -> (Objects.equals(getExpect(), actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s[%s], but was %s[%s]", fieldStr(),
                        getClassName(getExpect()), getExpect(), getClassName(actual.getInstance()), actual.getInstance()));
    }

    public Expectation typeExpectation() {
        return genericType(type -> (actual, runtimeContext) -> (type.getType().isInstance(actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s, but was [%s]", fieldStr(), inspectExpectType(),
                        getClassName(actual.getInstance()))));
    }

    private Expectation genericType(Function<BeanClass<?>, Expectation> method) {
        return method.apply(type.getTypeArguments(0).orElseThrow(() -> illegalStateException(fieldStr())));
    }

    public Expectation typeContentExpectation() {
        return (actual, runtimeContext) -> {
            Type<Object> expect = (Type<Object>) this.expect;
            return (expect.verify(actual.getInstance())) ||
                    errorLog(expect.errorMessage(fieldStr(), actual.getInstance()));
        };
    }

    public Expectation valueExpectation() {
        return genericType(type -> (actual, runtimeContext) -> {
            try {
                if (actual.isNull())
                    return errorLog("Can not convert null field `%s` to %s, " +
                            "use @AllowNull to verify nullable field", fieldStr(), inspectExpectType());
                actual.convert(type.getType());
                return true;
            } catch (Exception ignore) {
                return errorLog("Can not convert field `%s` (%s: %s) to %s", fieldStr(),
                        getClassName(actual.getInstance()), actual.getInstance(), inspectExpectType());
            }
        });
    }

    public Expectation valueContentExpectation() {
        return (actual, runtimeContext) -> {
            try {
                Value<Object> expect = (Value<Object>) this.expect;
                return expect.verify(expect.convertAs(actual, type.getTypeArguments(0).orElse(null)))
                        || errorLog(expect.errorMessage(fieldStr(), actual.getInstance()));
            } catch (IllegalFieldException ignore) {
                throw illegalStateException(fieldStr());
            }
        };
    }

    private Expectation formatterExpectationInner(Formatter<Object, Object> formatter) {
        return (actual, runtimeContext) -> formatter.isValid(actual.getInstance())
                || errorLog("Expecting field `%s` to be in `%s`, but was [%s]", fieldStr(),
                formatter.getFormatterName(), actual.getInstance());
    }

    public Expectation formatterContentExpectation() {
        return formatterExpectationInner((Formatter<Object, Object>) expect);
    }

    public Expectation formatterExpectation() {
        return formatterExpectationInner(createFormatter(type));
    }


    public Expectation schemaExpectation() {
        return schemaExpectationBase(type -> (actual, runtimeContext) ->
                allPropertyValueShouldBeValid(type, type.newInstance(), actual, runtimeContext));
    }

    @SuppressWarnings("unchecked")
    private Expectation schemaExpectationBase(Function<BeanClass<Object>, Expectation> method) {
        return (actual, runtimeContext) -> {
            BeanClass<Object> type = (BeanClass<Object>) getPolymorphicSchemaType(this.type.getType(), actual);
            return verifySchema(actual, type, this.type) && method.apply(type).verify(actual, runtimeContext);
        };
    }

    private boolean verifySchema(Data actual, BeanClass<?> type, BeanClass<?> originalType) {
        Set<String> propertyReaderNames = actual.getFieldNames().stream().filter(String.class::isInstance)
                .map(Object::toString).collect(toSet());
        return (originalType.getType().getAnnotation(Partial.class) != null ||
                noMoreUnexpectedField(propertyReaderNames, type))
                && allMandatoryPropertyShouldBeExist(propertyReaderNames, type);
    }

    public Expectation schemaContentExpectation() {
        return schemaExpectationBase(type -> (actual, runtimeContext) ->
                schemaVerificationShouldPass(expect, actual) &&
                        allPropertyValueShouldBeValid(type, expect, actual, runtimeContext)
        );
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

    private <T> boolean allPropertyValueShouldBeValid(BeanClass<T> schemaType, T schema, Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        return schemaType.getPropertyReaders().values().stream().allMatch(propertyReader -> {
            Data fieldValue = actual.getValue(propertyReader.getName());
            return SchemaVerifier.allowNullAndIsNull(propertyReader, fieldValue)
                    || createFieldSchema(fieldStr() + "." + propertyReader.getName(), propertyReader.getType(),
                    propertyReader.getValue(schema), runtimeContext, fieldValue)
                    .verify(runtimeContext);

        });
    }

    private boolean allMandatoryPropertyShouldBeExist(Set<String> propertyReaderNames, BeanClass<?> schemaType) {
        return schemaType.getPropertyReaders().values().stream()
                .filter(propertyReader -> propertyReader.getAnnotation(AllowNull.class) == null)
                .allMatch(propertyReader -> SchemaVerifier.shouldContainsField(propertyReaderNames, schemaType, propertyReader));
    }

    private boolean noMoreUnexpectedField(Set<String> propertyReaderNames, BeanClass<?> schemaType) {
        return propertyReaderNames.stream().allMatch(f -> shouldNotContainsUnexpectedField(schemaType, schemaType.getPropertyReaders().keySet(), f));
    }

    private static final Compiler compiler = new Compiler();

    public static BeanClass<?> getPolymorphicSchemaType(Class<?> schemaType, Data actual) {
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
}
