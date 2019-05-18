package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.Evaluatable;

import java.util.Objects;

public class Expression implements Evaluatable {
    private Object rightValue;

    public void setRightValue(Object rightValue) {
        this.rightValue = rightValue;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        return Objects.equals(context.getInputValue(), rightValue);
    }
}
