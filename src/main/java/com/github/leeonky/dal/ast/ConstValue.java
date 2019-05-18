package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

public class ConstValue implements Evaluatable {
    private final Object value;

    public ConstValue(Object value) {
        this.value = value;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        return value;
    }
}
