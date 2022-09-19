package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.dal.runtime.IllegalTypeException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.SchemaAssertionFailure;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.Schema;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.PropertyReader;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.runtime.schema.Assertion.IfFactory.when;
import static com.github.leeonky.dal.runtime.schema.Assertion.assertion;
import static com.github.leeonky.util.BeanClass.getClassName;
import static com.github.leeonky.util.CollectionHelper.toStream;
import static java.lang.Integer.parseInt;
import static java.lang.String.format;
import static java.lang.String.valueOf;
import static java.util.stream.Collectors.toSet;
import static java.util.stream.IntStream.range;

public class Verification {
    private static final Assertion.Factory<Expect, Assertion> SCHEMA_ASSERTIONS = when(Expect::isFormatter).then(assertion(Verification::formatterStructure, Verification::formatterContent))
            .when(Expect::isCollection).then(assertion(Verification::collectionStructure, Verification::collectionContent))
            .when(Expect::isMap).then(assertion(Verification::mapStructure, Verification::mapContent))
            .when(Expect::isSchemaValue).then(assertion(Verification::valueStructure, Verification::valueContent))
            .when(Expect::isSchema).then(assertion(Verification::schema))
            .when(Expect::isSchemaType).then(assertion(Verification::typeStructure, Verification::typeContent))
            .orElse(assertion(Verification::structure, Verification::content));
    final Expect expect;
    final String property;
    final Data actual;

    public Verification(Expect expect, String property, Data actual) {
        this.expect = expect;
        this.property = property;
        this.actual = actual;
    }

    static IllegalStateException illegalStateException(String subPrefix) {
        return new IllegalStateException(format("%s should specify generic type", subPrefix));
    }

    public static boolean errorLog(String format, Object... params) {
        throw new IllegalTypeException(format(format, params));
    }

    public Verification collectionElementExpectation(PropertyReader<Object> propertyReader, DALRuntimeContext context) {
        return subExpectation(propertyReader, property + "[" + propertyReader.getName() + "]", parseInt(propertyReader.getName()), context);
    }

    public Verification propertyExpectation(PropertyReader<Object> propertyReader, DALRuntimeContext context) {
        return subExpectation(propertyReader, property + "." + propertyReader.getName(), propertyReader.getName(), context);
    }

    //    TODO refactor ********************
    private Verification subExpectation(PropertyReader<Object> propertyReader, String propertyChain, Object property, DALRuntimeContext context) {
        return new Verification(expect.subExpect(propertyReader, context, actual.getValue(property)), propertyChain, actual.getValue(property));
    }

    private Verification mapEntryExpectation(Object key, DALRuntimeContext context) {
        return new Verification(
                expect.subExpect(key, context, expect, property, actual.getValue(key)), property + "." + key, actual.getValue(key));
    }

    public boolean verify(DALRuntimeContext runtimeContext, Expect expect) {
        return SCHEMA_ASSERTIONS
                .createBy(expect)
                .verify(runtimeContext, this);
    }

    public boolean structure() {
        return expect.getExpect() == null;
    }

    private boolean valueStructure(DALRuntimeContext runtimeContext) {
        BeanClass<?> type = expect.getType().getTypeArguments(0).orElseThrow(() -> illegalStateException(property));
        try {
            if (actual.isNull())
                return errorLog("Can not convert null field `%s` to %s, " +
                        "use @AllowNull to verify nullable field", property, inspectExpectType());
            actual.convert(type.getType());
            return true;
        } catch (Exception ignore) {
            return errorLog("Can not convert field `%s` (%s: %s) to %s", property,
                    getClassName(actual.getInstance()), actual.getInstance(), inspectExpectType());
        }
    }

    private boolean valueContent(DALRuntimeContext runtimeContext) {
        try {
            Value<Object> expect = (Value<Object>) this.expect.getExpect();
            return expect.verify(expect.convertAs(actual, this.expect.getType().getTypeArguments(0).orElse(null)))
                    || errorLog(expect.errorMessage(property, actual.getInstance()));
        } catch (IllegalFieldException ignore) {
            throw illegalStateException(property);
        }
    }

    private boolean mapStructure(DALRuntimeContext context) {
        return actual.getFieldNames().stream().allMatch(key -> {
            Verification verification = mapEntryExpectation(key, context);
            return verification.verify(context, verification.expect);
        });
    }

    private boolean mapContent(DALRuntimeContext context) {
        return verifySize(actual.getFieldNames().size(), ((Map<?, Object>) expect.getExpect()).size()) && mapStructure(context);
    }

    private boolean collectionStructure(DALRuntimeContext context) {
        return range(0, actual.getListSize()).allMatch(index ->
        {
            Verification verification = collectionElementExpectation(expect.getType().getPropertyReader(valueOf(index)), context);
            return verification.verify(context, verification.expect);
        });
    }

    private boolean collectionContent(DALRuntimeContext context) {
        return verifySize(actual.getListSize(), (int) toStream(expect.getExpect()).count()) && collectionStructure(context);
    }

    private boolean formatterContent(DALRuntimeContext runtimeContext) {
        return verifyFormatter((Formatter<Object, Object>) expect.getExpect());
    }

    private boolean formatterStructure(DALRuntimeContext runtimeContext) {
        return verifyFormatter(Formatter.createFormatter(expect.getType()));
    }

    private boolean verifySize(int actualSize, int expectSize) {
        return actualSize == expectSize || errorLog("Expecting field `%s` to be size [%d], but was size [%d]",
                property, expectSize, actualSize);
    }

    private boolean verifyFormatter(Formatter<Object, Object> formatter) {
        return formatter.isValid(actual.getInstance())
                || errorLog("Expecting field `%s` to be in `%s`, but was [%s]", property,
                formatter.getFormatterName(), actual.getInstance());
    }

    private boolean typeContent(DALRuntimeContext runtimeContext) {
        Type<Object> expect = (Type<Object>) this.expect.getExpect();
        return expect.verify(actual.getInstance()) ||
                errorLog(expect.errorMessage(property, actual.getInstance()));
    }

    private String inspectExpectType() {
        return format("type [%s]", expect.getType().getName());
    }

    private boolean typeStructure(DALRuntimeContext r) {
        BeanClass<?> type1 = expect.getType().getTypeArguments(0).orElseThrow(() -> illegalStateException(property));
        return type1.getType().isInstance(actual.getInstance()) ||
                errorLog(format("Expecting field `%s` to be %s, but was [%s]", property, inspectExpectType(),
                        getClassName(actual.getInstance())));
    }

    private boolean structure(DALRuntimeContext runtimeContext) {
        return expect.getType().getType().isInstance(actual.getInstance())
                || errorLog(format("Expecting field `%s` to be %s, but was [%s]", property, inspectExpectType(),
                getClassName(actual.getInstance())));
    }

    private boolean content(DALRuntimeContext runtimeContext) {
        return Objects.equals(expect.getExpect(), actual.getInstance()) ||
                errorLog(format("Expecting field `%s` to be %s[%s], but was %s[%s]", property,
                        getClassName(expect.getExpect()), expect.getExpect(), getClassName(actual.getInstance()), actual.getInstance()));
    }

    protected boolean schema(DALRuntimeContext runtimeContext) {
        Set<String> actualFields = actual.getFieldNames().stream().filter(String.class::isInstance)
                .map(Object::toString).collect(toSet());
        return (expect.isPartial() || noMoreUnexpectedField(actualFields))
                && allMandatoryPropertyShouldBeExist(actualFields)
                && allPropertyValueShouldBeValid(actual, runtimeContext)
                && schemaVerificationShouldPass(expect.getExpect(), actual);
    }

    private boolean allMandatoryPropertyShouldBeExist(Set<String> actualFields) {
        return expect.getType().getPropertyReaders().values().stream()
                .filter(field -> field.getAnnotation(AllowNull.class) == null)
                .allMatch(field -> actualFields.contains(field.getName())
                        || errorLog("Expecting field `%s` to be in type %s[%s], but does not exist", field.getName(),
                        expect.getType().getSimpleName(), expect.getType().getName()));
    }

    private boolean allPropertyValueShouldBeValid(Data actual, DALRuntimeContext runtimeContext) {
        return expect.getType().getPropertyReaders().values().stream().allMatch(propertyReader -> {
            Data subActual = actual.getValue(propertyReader.getName());
            if (allowNullAndIsNull(propertyReader, subActual)) return true;
            Verification verification = propertyExpectation(propertyReader, runtimeContext);
            return verification.verify(runtimeContext, verification.expect);
        });
    }

    private boolean allowNullAndIsNull(PropertyReader<?> propertyReader, Data propertyValueWrapper) {
        return propertyReader.getAnnotation(AllowNull.class) != null && propertyValueWrapper.isNull();
    }

    private boolean noMoreUnexpectedField(Set<String> actualFields) {
        Set<String> expectFields = new LinkedHashSet<String>(actualFields) {{
            removeAll(expect.getType().getPropertyReaders().keySet());
        }};
        return expectFields.isEmpty() || errorLog("Unexpected field %s for schema %s[%s]",
                expectFields.stream().collect(Collectors.joining("`, `", "`", "`")),
                expect.getType().getSimpleName(), expect.getType().getName());
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
