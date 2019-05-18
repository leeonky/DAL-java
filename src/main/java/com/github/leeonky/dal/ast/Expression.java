package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.AssertResult;
import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.Evaluatable;

import java.util.Objects;

public class Expression implements Evaluatable {
    private Object rightValue;

    public void setRightValue(Object rightValue) {
        this.rightValue = rightValue;
    }

    @Override
    public AssertResult evaluate(CompilingContext context) {
        AssertResult assertResult = new AssertResult();
        if (Objects.equals(context.getInputValue(), rightValue))
            return assertResult;
        return assertResult.setMessage("Expected value to [= 2]\n but was <" + context.getInputValue() + ">");
    }
}
