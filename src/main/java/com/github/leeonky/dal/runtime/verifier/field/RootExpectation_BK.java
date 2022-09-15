package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.util.BeanClass;

import java.util.Objects;
import java.util.function.Function;

import static com.github.leeonky.dal.format.Formatter.createFormatter;
import static com.github.leeonky.dal.runtime.verifier.field.Factory.illegalStateException;
import static com.github.leeonky.dal.runtime.verifier.field.SchemaExpectation.errorLog;
import static com.github.leeonky.util.BeanClass.getClassName;
import static java.lang.String.format;

@Deprecated
public class RootExpectation_BK<T> {
    protected final Object property;
    protected final T expect;
    protected final BeanClass<?> type;

    public RootExpectation_BK(BeanClass<?> type, Object property, T expect) {
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

    public Expectation_BK structureExpectation() {
        return (actual, runtimeContext) -> type.getType().isInstance(actual.getInstance())
                || errorLog(format("Expecting field `%s` to be %s, but was [%s]", fieldStr(), inspectExpectType(),
                getClassName(actual.getInstance())));
    }

    public Expectation_BK contentExpectation() {
        return (actual, runtimeContext) -> (Objects.equals(getExpect(), actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s[%s], but was %s[%s]", fieldStr(),
                        getClassName(getExpect()), getExpect(), getClassName(actual.getInstance()), actual.getInstance()));
    }

    public Expectation_BK typeExpectation() {
        return genericType(type -> (actual, runtimeContext) -> (type.getType().isInstance(actual.getInstance())) ||
                errorLog(format("Expecting field `%s` to be %s, but was [%s]", fieldStr(), inspectExpectType(),
                        getClassName(actual.getInstance()))));
    }

    private Expectation_BK genericType(Function<BeanClass<?>, Expectation_BK> method) {
        return method.apply(type.getTypeArguments(0).orElseThrow(() -> illegalStateException(fieldStr())));
    }

    public Expectation_BK typeContentExpectation() {
        return (actual, runtimeContext) -> {
            Type<Object> expect = (Type<Object>) this.expect;
            return (expect.verify(actual.getInstance())) ||
                    errorLog(expect.errorMessage(fieldStr(), actual.getInstance()));
        };
    }

    public Expectation_BK valueExpectation() {
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

    public Expectation_BK valueContentExpectation() {
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

    private Expectation_BK formatterExpectationInner(Formatter<Object, Object> formatter) {
        return (actual, runtimeContext) -> formatter.isValid(actual.getInstance())
                || errorLog("Expecting field `%s` to be in `%s`, but was [%s]", fieldStr(),
                formatter.getFormatterName(), actual.getInstance());
    }

    public Expectation_BK formatterContentExpectation() {
        return formatterExpectationInner((Formatter<Object, Object>) expect);
    }

    public Expectation_BK formatterExpectation() {
        return formatterExpectationInner(createFormatter(type));
    }


    public Expectation_BK schemaExpectation() {
        return (actual, runtimeContext) -> {
            new SchemaExpectation(fieldStr(), type, actual).verify(actual, runtimeContext);
            return true;
        };
    }

    public Expectation_BK schemaContentExpectation() {
        return (actual, runtimeContext) -> {
            new SchemaContentExpectation(fieldStr(), type, actual, expect).verify(actual, runtimeContext);
            return true;
        };
    }


}
