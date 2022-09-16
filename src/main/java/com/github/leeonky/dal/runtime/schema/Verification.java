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
    final BeanClass<Object> type;
    final String property;
    final Object expect;
    final Data actual;

    public Verification(BeanClass<Object> type, String property, Object expect, Data actual) {
        this.type = type;
        this.property = property;
        this.expect = expect;
        this.actual = actual;
    }

    static IllegalStateException illegalStateException(String subPrefix) {
        return new IllegalStateException(format("%s should specify generic type", subPrefix));
    }

    public Verification collectionElementExpectation(PropertyReader<Object> propertyReader) {
        return subExpectation(propertyReader, property + "[" + propertyReader.getName() + "]", parseInt(propertyReader.getName()));
    }

    public Verification propertyExpectation(PropertyReader<Object> propertyReader) {
        return subExpectation(propertyReader, property + "." + propertyReader.getName(), propertyReader.getName());
    }

    private Verification subExpectation(PropertyReader<Object> propertyReader, String property, Object pro) {
        return new Verification((BeanClass<Object>) propertyReader.getType(), property,
                structure() ? null : propertyReader.getValue(expect), actual.getValue(pro));
    }

    @SuppressWarnings("unchecked")
    private Verification mapEntryExpectation(Object key) {
        return new Verification((BeanClass<Object>) type.getTypeArguments(1).orElseThrow(() ->
                new IllegalArgumentException(format("`%s` should be generic type", property))),
                property + "." + key, structure() ? null : ((Map<?, Object>) expect).get(key), actual.getValue(key));
    }

    @Override
    public boolean verify(DALRuntimeContext runtimeContext) {
        return new ArrayList<Assertion>() {{
            add(when(Verification::isFormatter).then(Verification::formatterStructure, Verification::formatterContent));
            add(when(Verification::isCollection).then(Verification::collectionStructure, Verification::collectionContent));
            add(when(Verification::isMap).then(Verification::mapStructure, Verification::mapContent));
            add(when(Verification::isSchemaValue).then(Verification::valueStructure, Verification::valueContent));

        }}.stream().filter(assertion -> assertion.matches(this)).findFirst()
                .map(assertion -> assertion.verify(runtimeContext, this))
                .orElseGet(() -> delegateAll(actual, runtimeContext).verify(runtimeContext));
    }

    public boolean structure() {
        return expect == null;
    }

    public Expectation delegateAll(Data actual, DALRuntimeContext runtimeContext) {
        if (structure()) {
            if (runtimeContext.isSchemaRegistered(((BeanClass<?>) type).getType())) {
                return new SchemaVerification(property, type, actual);
            }
            if (Type.class.isAssignableFrom(((BeanClass<?>) type).getType())) {
                return typeExpectation();
            }
            return structureExpectation();
        }
        if (runtimeContext.isSchemaRegistered(((BeanClass<?>) type).getType()))
            return new SchemaVerification(property, type, actual, expect);
        if (Type.class.isAssignableFrom(((BeanClass<?>) type).getType()))
            return typeContentExpectation();
        return contentExpectation();
    }

    private boolean isSchemaValue() {
        return Value.class.isAssignableFrom(((BeanClass<?>) type).getType());
    }

    private boolean valueStructure(DALRuntimeContext runtimeContext) {
        BeanClass<?> type = this.type.getTypeArguments(0).orElseThrow(() -> illegalStateException(property));
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
            Value<Object> expect = (Value<Object>) this.expect;
            return expect.verify(expect.convertAs(actual, type.getTypeArguments(0).orElse(null)))
                    || errorLog(expect.errorMessage(property, actual.getInstance()));
        } catch (IllegalFieldException ignore) {
            throw illegalStateException(property);
        }
    }

    private boolean isMap() {
        return Map.class.isAssignableFrom(((BeanClass<?>) type).getType());
    }

    private boolean mapStructure(DALRuntimeContext context) {
        return actual.getFieldNames().stream().allMatch(key -> mapEntryExpectation(key).verify(context));
    }

    private boolean mapContent(DALRuntimeContext context) {
        return verifySize(actual.getFieldNames().size(), ((Map<?, Object>) expect).size()) && mapStructure(context);
    }

    private boolean isCollection() {
        return type.isCollection();
    }

    private boolean collectionStructure(DALRuntimeContext context) {
        return range(0, actual.getListSize()).allMatch(index ->
                collectionElementExpectation(type.getPropertyReader(valueOf(index))).verify(context));
    }

    private boolean collectionContent(DALRuntimeContext context) {
        return verifySize(actual.getListSize(), (int) toStream(expect).count()) && collectionStructure(context);
    }

    private boolean isFormatter() {
        return Formatter.class.isAssignableFrom(((BeanClass<?>) type).getType());
    }

    private boolean formatterContent(DALRuntimeContext runtimeContext) {
        return verifyFormatter((Formatter<Object, Object>) expect);
    }

    private boolean formatterStructure(DALRuntimeContext runtimeContext) {
        return verifyFormatter(Formatter.createFormatter(type));
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
            Type<Object> expect = (Type<Object>) this.expect;
            return (expect.verify(actual.getInstance())) ||
                    errorLog(expect.errorMessage(property, actual.getInstance()));
        };
    }

    private String inspectExpectType() {
        return format("type [%s]", type.getName());
    }

    private Expectation genericType(Function<BeanClass<?>, Expectation> method) {
        return method.apply(type.getTypeArguments(0).orElseThrow(() -> illegalStateException(property)));
    }

    private Expectation typeExpectation() {
        return genericType(type -> (runtimeContext) -> (type.getType().isInstance(actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s, but was [%s]", property, inspectExpectType(),
                        getClassName(actual.getInstance()))));
    }

    private Expectation structureExpectation() {
        return (runtimeContext) -> type.getType().isInstance(actual.getInstance())
                || errorLog(format("Expecting field `%s` to be %s, but was [%s]", property, inspectExpectType(),
                getClassName(actual.getInstance())));
    }

    private Expectation contentExpectation() {
        return (runtimeContext) -> (Objects.equals(expect, actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s[%s], but was %s[%s]", property,
                        getClassName(expect), expect, getClassName(actual.getInstance()), actual.getInstance()));
    }
}
