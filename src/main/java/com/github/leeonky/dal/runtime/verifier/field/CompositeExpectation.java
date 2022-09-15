package com.github.leeonky.dal.runtime.verifier.field;

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

import static com.github.leeonky.dal.runtime.verifier.field.Factory.illegalStateException;
import static com.github.leeonky.dal.runtime.verifier.field.SchemaExpectation.errorLog;
import static com.github.leeonky.util.BeanClass.getClassName;
import static com.github.leeonky.util.CollectionHelper.toStream;
import static java.lang.String.format;
import static java.lang.String.valueOf;
import static java.util.stream.IntStream.range;

public class CompositeExpectation implements Expectation {
    final BeanClass<Object> type;
    final String property;
    final Object expect;

    public Object getProperty() {
        return property;
    }

    public Object getExpect() {
        return expect;
    }

    protected String fieldStr() {
        return property;
    }


    public CompositeExpectation(BeanClass<Object> type, String property, Object expect) {
        this.type = type;
        this.property = property;
        this.expect = expect;
    }

    public CompositeExpectation subExpectation(PropertyReader<Object> propertyReader) {
        return new CompositeExpectation((BeanClass<Object>) propertyReader.getType(),
                propertyReader.getBeanType().isCollection() ? property + "[" + propertyReader.getName() + "]"
                        : property + "." + propertyReader.getName(), propertyReader.getValue(expect));
    }

    public CompositeExpectation mapEntryExpectation(Object key) {
        return new CompositeExpectation((BeanClass<Object>) type.getTypeArguments(1).orElseThrow(() ->
                new IllegalArgumentException(format("`%s` should be generic type", property))),
                format("%s.%s", property, key), ((Map<?, Object>) expect).get(key));
    }

    @Override
    public boolean verify(Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        RootExpectation_BK<Object> expectation = new RootExpectation_BK<>(type, property, expect);

        return (expect == null ? Factory.createSchema(property, type, expect, actual, expectation, runtimeContext).verify(runtimeContext)
                : delegate(actual, runtimeContext).verify(actual, runtimeContext));
    }

    private Expectation delegate(Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        if (Formatter.class.isAssignableFrom(((BeanClass<?>) type).getType()))
            return formatterExpectation((Formatter<Object, Object>) expect);
        if (runtimeContext.isSchemaRegistered(((BeanClass<?>) type).getType()))
            return new SchemaContentExpectation(fieldStr(), type, actual, expect);
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

    private Expectation mapContentExpectation() {
        return (actual, runtimeContext) -> verifySize(actual.getFieldNames().size(), ((Map<?, Object>) expect).size())
                && actual.getFieldNames().stream().allMatch(key ->
                mapEntryExpectation(key).verify(actual.getValue(key), runtimeContext));
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
                || errorLog("Expecting field `%s` to be in `%s`, but was [%s]", fieldStr(),
                formatter.getFormatterName(), actual.getInstance());
    }

    private Expectation valueContentExpectation() {
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

    private Expectation typeContentExpectation() {
        return (actual, runtimeContext) -> {
            Type<Object> expect = (Type<Object>) this.expect;
            return (expect.verify(actual.getInstance())) ||
                    errorLog(expect.errorMessage(fieldStr(), actual.getInstance()));
        };
    }

    private Expectation contentExpectation() {
        return (actual, runtimeContext) -> (Objects.equals(expect, actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s[%s], but was %s[%s]", fieldStr(),
                        getClassName(expect), expect, getClassName(actual.getInstance()), actual.getInstance()));
    }
}
