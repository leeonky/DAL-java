package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.runtime.Data;

import java.util.Objects;

import static com.github.leeonky.util.BeanClass.getClassName;

public class ContentExpectation<T> extends Expectation {
    protected final T expect;

    public ContentExpectation(Object property, T expect) {
        super(property);
        this.expect = expect;
    }

    public T getExpect() {
        return expect;
    }

    @Override
    protected String inspectExpect() {
        return inspect(expect);
    }

    private String inspect(Object value) {
        return String.format("%s[%s]", getClassName(value), value);
    }

    @Override
    protected String inspectActual(Data actual) {
        return inspect(actual.getInstance());
    }

    @Override
    protected boolean doVerify(Data actual) {
        return Objects.equals(expect, actual.getInstance());
    }

    public static class SchemaType extends ContentExpectation<Type<Object>> {
        public SchemaType(String property, Type<Object> expect) {
            super(property, expect);
        }

        @Override
        public boolean doVerify(Data actual) {
            return getExpect().verify(actual.getInstance());
        }

        @Override
        protected String failedMessage(Data actual) {
            return getExpect().errorMessage((String) getProperty(), actual.getInstance());
        }
    }
}
