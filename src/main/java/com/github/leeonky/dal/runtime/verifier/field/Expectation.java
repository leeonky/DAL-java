package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.runtime.Data;

import static com.github.leeonky.dal.runtime.verifier.SchemaVerifier.errorLog;

public abstract class Expectation {
    protected final Object property;

    public Expectation(Object property) {
        this.property = property;
    }

    public Object getProperty() {
        return property;
    }

    public boolean verify(Data actual) {
        return doVerify(actual) || errorLog("Expecting field `%s` to be %s, but was %s",
                fieldStr(), inspectExpect(), inspectActual(actual));
    }

    protected abstract String inspectExpect();

    protected abstract String inspectActual(Data actual);

    protected abstract boolean doVerify(Data actual);

    protected String fieldStr() {
        return "" + property;
    }
}
