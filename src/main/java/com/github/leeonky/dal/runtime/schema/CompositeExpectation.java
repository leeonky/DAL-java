package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.PropertyReader;

import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

import static com.github.leeonky.dal.runtime.schema.SchemaExpectation.errorLog;
import static com.github.leeonky.util.BeanClass.getClassName;
import static com.github.leeonky.util.CollectionHelper.toStream;
import static java.lang.String.format;
import static java.lang.String.valueOf;
import static java.util.stream.IntStream.range;

public class CompositeExpectation implements Expectation {
    final BeanClass<Object> type;
    final String property;
    final Object expect;

    public CompositeExpectation(BeanClass<Object> type, String property, Object expect) {
        this.type = type;
        this.property = property;
        this.expect = expect;
    }

    static IllegalStateException illegalStateException(String subPrefix) {
        return new IllegalStateException(format("%s should specify generic type", subPrefix));
    }

    public CompositeExpectation subExpectation(PropertyReader<Object> propertyReader) {
        return new CompositeExpectation((BeanClass<Object>) propertyReader.getType(),
                propertyReader.getBeanType().isCollection() ? property + "[" + propertyReader.getName() + "]"
                        : property + "." + propertyReader.getName(), expect == null ? null : propertyReader.getValue(expect));
    }

    private CompositeExpectation mapEntryExpectation(Object key) {
        return new CompositeExpectation((BeanClass<Object>) type.getTypeArguments(1).orElseThrow(() ->
                new IllegalArgumentException(format("`%s` should be generic type", property))),
                format("%s.%s", property, key), expect == null ? null : ((Map<?, Object>) expect).get(key));
    }

    @Override
    public boolean verify(Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        return (expect == null ? delegate1(actual, runtimeContext).verify(actual, runtimeContext)
                : delegate(actual, runtimeContext).verify(actual, runtimeContext));
    }

    private Expectation delegate1(Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        Expectation expectation1;
        if (Formatter.class.isAssignableFrom(((BeanClass<?>) type).getType())) {
            expectation1 = formatterExpectation(Formatter.createFormatter(type));
        } else if (runtimeContext.isSchemaRegistered(((BeanClass<?>) type).getType())) {
            expectation1 = new SchemaExpectation(property, type, actual);
        } else if (type.isCollection()) {
            expectation1 = collectionExpectation();
        } else if (Map.class.isAssignableFrom(((BeanClass<?>) type).getType())) {
            expectation1 = mapExpectation();
        } else if (Value.class.isAssignableFrom(((BeanClass<?>) type).getType())) {
            expectation1 = valueExpectation();
        } else if (Type.class.isAssignableFrom(((BeanClass<?>) type).getType())) {
            expectation1 = typeExpectation();
        } else {
            expectation1 = structureExpectation();
        }
        return expectation1;
    }

    private Expectation delegate(Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        if (Formatter.class.isAssignableFrom(((BeanClass<?>) type).getType()))
            return formatterExpectation((Formatter<Object, Object>) expect);
        if (runtimeContext.isSchemaRegistered(((BeanClass<?>) type).getType()))
            return new SchemaContentExpectation(property, type, actual, expect);
        if (type.isCollection())
            return collectionContentExpectation();
        if (Map.class.isAssignableFrom(((BeanClass<?>) type).getType()))
            return mapContentExpectation();
        if (Value.class.isAssignableFrom(((BeanClass<?>) type).getType()))
            return valueContentExpectation();
        if (Type.class.isAssignableFrom(((BeanClass<?>) type).getType()))
            return typeContentExpectation();
        return contentExpectation();
    }

    private Expectation mapExpectation() {
        return (actual, runtimeContext) -> actual.getFieldNames().stream().allMatch(key ->
                mapEntryExpectation(key).verify(actual.getValue(key), runtimeContext));
    }

    private Expectation mapContentExpectation() {
        return (actual, runtimeContext) -> verifySize(actual.getFieldNames().size(), ((Map<?, Object>) expect).size())
                && actual.getFieldNames().stream().allMatch(key -> mapEntryExpectation(key).verify(actual.getValue(key), runtimeContext));
    }

    private Expectation collectionExpectation() {
        return (actual, runtimeContext) -> range(0, actual.getListSize()).allMatch(property ->
                subExpectation(type.getPropertyReader(valueOf(property)))
                        .verify(actual.getValue(property), runtimeContext));
    }

    private Expectation collectionContentExpectation() {
        return (actual, runtimeContext) -> verifySize(actual.getListSize(), (int) toStream(expect).count())
                && range(0, actual.getListSize()).allMatch(property ->
                subExpectation(type.getPropertyReader(valueOf(property)))
                        .verify(actual.getValue(property), runtimeContext));
    }

    private boolean verifySize(int actualSize, int expectSize) {
        return actualSize == expectSize || errorLog("Expecting field `%s` to be size [%d], but was size [%d]",
                property, expectSize, actualSize);
    }

    private Expectation formatterExpectation(Formatter<Object, Object> formatter) {
        return (actual, runtimeContext) -> formatter.isValid(actual.getInstance())
                || errorLog("Expecting field `%s` to be in `%s`, but was [%s]", property,
                formatter.getFormatterName(), actual.getInstance());
    }

    private Expectation valueContentExpectation() {
        return (actual, runtimeContext) -> {
            try {
                Value<Object> expect = (Value<Object>) this.expect;
                return expect.verify(expect.convertAs(actual, type.getTypeArguments(0).orElse(null)))
                        || errorLog(expect.errorMessage(property, actual.getInstance()));
            } catch (IllegalFieldException ignore) {
                throw illegalStateException(property);
            }
        };
    }

    private Expectation typeContentExpectation() {
        return (actual, runtimeContext) -> {
            Type<Object> expect = (Type<Object>) this.expect;
            return (expect.verify(actual.getInstance())) ||
                    errorLog(expect.errorMessage(property, actual.getInstance()));
        };
    }

    private Expectation contentExpectation() {
        return (actual, runtimeContext) -> (Objects.equals(expect, actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s[%s], but was %s[%s]", property,
                        getClassName(expect), expect, getClassName(actual.getInstance()), actual.getInstance()));
    }

    private String inspectExpectType() {
        return format("type [%s]", type.getName());
    }

    private Expectation structureExpectation() {
        return (actual, runtimeContext) -> type.getType().isInstance(actual.getInstance())
                || errorLog(format("Expecting field `%s` to be %s, but was [%s]", property, inspectExpectType(),
                getClassName(actual.getInstance())));
    }

    private Expectation genericType(Function<BeanClass<?>, Expectation> method) {
        return method.apply(type.getTypeArguments(0).orElseThrow(() -> illegalStateException(property)));
    }

    private Expectation typeExpectation() {
        return genericType(type -> (actual, runtimeContext) -> (type.getType().isInstance(actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s, but was [%s]", property, inspectExpectType(),
                        getClassName(actual.getInstance()))));
    }

    private Expectation valueExpectation() {
        return genericType(type -> (actual, runtimeContext) -> {
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
        });
    }
}
