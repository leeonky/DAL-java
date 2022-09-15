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

    public BeanClass<?> getType() {
        return type;
    }


    public CompositeExpectation(BeanClass<Object> type, String property, Object expect) {
        this.type = type;
        this.property = property;
        this.expect = expect;
    }

    protected String fieldStr() {
        return property;
    }

    protected CompositeExpectation subExpectation(PropertyReader<Object> propertyReader) {
        String subProperty = propertyReader.getBeanType().isCollection() ? property + "[" + propertyReader.getName() + "]" : property + "." + propertyReader.getName();
        BeanClass<Object> subType = (BeanClass<Object>) propertyReader.getType();
        Object subExpect = propertyReader.getValue(expect);
        return new CompositeExpectation(subType, subProperty, subExpect);
    }

    @Override
    public boolean verify(Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        RootExpectation_BK<Object> expectation = new RootExpectation_BK<>(type, property, expect);
        return (expect == null ? Factory.createSchema(property, type, expect, actual, expectation, runtimeContext).verify(runtimeContext)
                : verifyContent(actual, runtimeContext, expectation))
                ;
    }

    private boolean verifyContent(Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext, RootExpectation_BK<Object> expectation) {
        FieldSchema_BK result = null;
        Expectation expectation1 = null;
        if (Formatter.class.isAssignableFrom(((BeanClass<?>) type).getType())) {
            expectation1 = formatterExpectation((Formatter<Object, Object>) expect);
        } else if (runtimeContext.isSchemaRegistered(((BeanClass<?>) type).getType())) {
            expectation1 = new SchemaContentExpectation(fieldStr(), type, actual, expect);
        } else if (type.isCollection()) {
            expectation1 = collectionContentExpectation();
        } else if (Map.class.isAssignableFrom(((BeanClass<?>) type).getType())) {
            result = new MapSchemaBK.MapContentSchemaBK(property, type, expect, actual);
        } else if (Value.class.isAssignableFrom(expectation.getType().getType())) {
            expectation1 = valueContentExpectation();
        } else if (Type.class.isAssignableFrom(expectation.getType().getType())) {
            expectation1 = typeContentExpectation();
        } else {
            expectation1 = contentExpectation();
        }
        if (expectation1 != null)
            return expectation1.verify(actual, runtimeContext);
        return result.verify(runtimeContext);

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
        return (actual, runtimeContext) -> (Objects.equals(getExpect(), actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s[%s], but was %s[%s]", fieldStr(),
                        getClassName(getExpect()), getExpect(), getClassName(actual.getInstance()), actual.getInstance()));
    }
}
