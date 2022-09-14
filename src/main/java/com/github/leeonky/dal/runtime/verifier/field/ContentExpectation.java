package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.runtime.Data;

import java.util.Objects;

import static com.github.leeonky.util.BeanClass.getClassName;

public class ContentExpectation extends Expectation {
    protected final Object expect;

    public ContentExpectation(Object property, Object expect) {
        super(property);
        this.expect = expect;
    }

    public Object getExpect() {
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
}
