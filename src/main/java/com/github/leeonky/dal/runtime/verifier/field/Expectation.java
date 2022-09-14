package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.IllegalTypeException;

import static java.lang.String.format;

public abstract class Expectation {
    protected final Object property;

    public Expectation(Object property) {
        this.property = property;
    }

    public Object getProperty() {
        return property;
    }

    public boolean verify(Data actual) {
        if (doVerify(actual)) return true;
        throw new IllegalTypeException(failedMessage(actual));
    }

    protected String failedMessage(Data actual) {
        return format("Expecting field `%s` to be %s, but was %s", fieldStr(), inspectExpect(), inspectActual(actual));
    }

    protected abstract String inspectExpect();

    protected abstract String inspectActual(Data actual);

    protected abstract boolean doVerify(Data actual);

    protected String fieldStr() {
        return "" + property;
    }
}
