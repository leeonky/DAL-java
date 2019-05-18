package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

import java.util.Objects;

public class Expression implements Evaluatable {
    private final Evaluatable leftValue, rightValue;

    public Expression(Evaluatable leftValue, Evaluatable rightValue) {
        this.leftValue = leftValue;
        this.rightValue = rightValue;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        return Objects.equals(leftValue.evaluate(context), rightValue.evaluate(context));
    }
}
