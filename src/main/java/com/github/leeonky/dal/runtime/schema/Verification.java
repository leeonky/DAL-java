package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.PropertyReader;

import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

import static com.github.leeonky.dal.runtime.schema.Assertion.PredicateAble.when;
import static com.github.leeonky.dal.runtime.schema.SchemaVerification.errorLog;
import static com.github.leeonky.util.BeanClass.getClassName;
import static com.github.leeonky.util.CollectionHelper.toStream;
import static java.lang.Integer.parseInt;
import static java.lang.String.format;
import static java.lang.String.valueOf;
import static java.util.stream.IntStream.range;

public class Verification implements Expectation {
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

    public Verification collectionElementExpectation(PropertyReader<Object> propertyReader, DALRuntimeContext context) {
        return subExpectation(propertyReader, property + "[" + propertyReader.getName() + "]", parseInt(propertyReader.getName()), context);
    }

    public Verification propertyExpectation(PropertyReader<Object> propertyReader, DALRuntimeContext context) {
        return subExpectation(propertyReader, property + "." + propertyReader.getName(), propertyReader.getName(), context);
    }

    private Verification subExpectation(PropertyReader<Object> propertyReader, String property, Object pro, DALRuntimeContext context) {
        return new Verification(new Expect((BeanClass<Object>) propertyReader.getType(),
                structure() ? null : propertyReader.getValue(expect.getExpect())) {
            @Override
            public boolean isSchema() {
                return context.isSchemaRegistered(getType().getType());
            }
        }, property,
                actual.getValue(pro));
    }

    @SuppressWarnings("unchecked")
    private Verification mapEntryExpectation(Object key, DALRuntimeContext context) {
        return new Verification(
                new Expect((BeanClass<Object>) expect.getType().getTypeArguments(1).orElseThrow(() ->
                        new IllegalArgumentException(format("`%s` should be generic type", property))),
                        structure() ? null : ((Map<?, Object>) expect.getExpect()).get(key)) {
                    @Override
                    public boolean isSchema() {
                        return context.isSchemaRegistered(getType().getType());
                    }
                }, property + "." + key, actual.getValue(key));
    }

    @Override
    public boolean verify(DALRuntimeContext runtimeContext) {
        return new ArrayList<Assertion>() {{
            add(when(Verification::isFormatter).then(Verification::formatterStructure, Verification::formatterContent));
            add(when(Verification::isCollection).then(Verification::collectionStructure, Verification::collectionContent));
            add(when(Verification::isMap).then(Verification::mapStructure, Verification::mapContent));
            add(when(Verification::isSchemaValue).then(Verification::valueStructure, Verification::valueContent));
            add(when(Verification::isSchema).then(Verification::schemaStructure, Verification::schemaContent));
        }}.stream().filter(assertion -> assertion.matches(this)).findFirst()
                .map(assertion -> assertion.verify(runtimeContext, this))
                .orElseGet(() -> delegateAll().verify(runtimeContext));
    }

    public boolean structure() {
        return expect.getExpect() == null;
    }

    public Expectation delegateAll() {
        if (structure()) {
            if (isSchemaType()) {
                return typeExpectation();
            }
            return structureExpectation();
        }
        if (isSchemaType())
            return typeContentExpectation();
        return contentExpectation();
    }

    private boolean isSchemaType() {
        return Type.class.isAssignableFrom(((BeanClass<?>) expect.getType()).getType());
    }

    private boolean isSchema() {
        return expect.isSchema();
    }

    private boolean schemaContent(DALRuntimeContext context) {
        return new SchemaVerification(property, expect.getType(), actual, expect.getExpect()).verify(context);
    }

    private boolean schemaStructure(DALRuntimeContext context) {
        return new SchemaVerification(property, expect.getType(), actual).verify(context);
    }

    private boolean isSchemaValue() {
        return Value.class.isAssignableFrom(((BeanClass<?>) expect.getType()).getType());
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

    private boolean isMap() {
        return Map.class.isAssignableFrom(((BeanClass<?>) expect.getType()).getType());
    }

    private boolean mapStructure(DALRuntimeContext context) {
        return actual.getFieldNames().stream().allMatch(key -> mapEntryExpectation(key, context).verify(context));
    }

    private boolean mapContent(DALRuntimeContext context) {
        return verifySize(actual.getFieldNames().size(), ((Map<?, Object>) expect.getExpect()).size()) && mapStructure(context);
    }

    private boolean isCollection() {
        return expect.getType().isCollection();
    }

    private boolean collectionStructure(DALRuntimeContext context) {
        return range(0, actual.getListSize()).allMatch(index ->
                collectionElementExpectation(expect.getType().getPropertyReader(valueOf(index)), context).verify(context));
    }

    private boolean collectionContent(DALRuntimeContext context) {
        return verifySize(actual.getListSize(), (int) toStream(expect.getExpect()).count()) && collectionStructure(context);
    }

    private boolean isFormatter() {
        return Formatter.class.isAssignableFrom(((BeanClass<?>) expect.getType()).getType());
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

    private Expectation typeContentExpectation() {
        return (runtimeContext) -> {
            Type<Object> expect = (Type<Object>) this.expect.getExpect();
            return (expect.verify(actual.getInstance())) ||
                    errorLog(expect.errorMessage(property, actual.getInstance()));
        };
    }

    private String inspectExpectType() {
        return format("type [%s]", expect.getType().getName());
    }

    private Expectation genericType(Function<BeanClass<?>, Expectation> method) {
        return method.apply(expect.getType().getTypeArguments(0).orElseThrow(() -> illegalStateException(property)));
    }

    private Expectation typeExpectation() {
        return genericType(type -> (runtimeContext) -> (type.getType().isInstance(actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s, but was [%s]", property, inspectExpectType(),
                        getClassName(actual.getInstance()))));
    }

    private Expectation structureExpectation() {
        return (runtimeContext) -> expect.getType().getType().isInstance(actual.getInstance())
                || errorLog(format("Expecting field `%s` to be %s, but was [%s]", property, inspectExpectType(),
                getClassName(actual.getInstance())));
    }

    private Expectation contentExpectation() {
        return (runtimeContext) -> (Objects.equals(expect.getExpect(), actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s[%s], but was %s[%s]", property,
                        getClassName(expect.getExpect()), expect.getExpect(), getClassName(actual.getInstance()), actual.getInstance()));
    }
}
