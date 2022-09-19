package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.runtime.Data;

public class Actual {
    private final String property;
    private final Data actual;

    public Actual(String property, Data actual) {
        this.property = property;
        this.actual = actual;
    }

    public String getProperty() {
        return property;
    }

    public Data getActual() {
        return actual;
    }
}
