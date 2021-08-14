package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.util.Calculator;

import java.util.Objects;

import static com.github.leeonky.util.BeanClass.getClassName;

public abstract class Node {
    private int positionBegin;

    public Object evaluate(RuntimeContext context) {
        throw new IllegalStateException();
    }

    public boolean judge(Operator.Equal operator, Object input, RuntimeContext context) {
        return Calculator.equals(input, evaluate(context));
    }

    public boolean judge(Operator.Matcher operator, Object input, RuntimeContext context) {
        Object value = evaluate(context);
        if (value == null)
            return Objects.equals(input, null);
        shouldBeSameTypeIfTypeIs(Number.class, input, value, operator);
        shouldBeSameTypeIfTypeIs(Boolean.class, input, value, operator);
        invalidTypeToMatchStringValue(Number.class, input, value, operator);
        invalidTypeToMatchStringValue(Boolean.class, input, value, operator);
        return Calculator.equals(context.getConverter().convert(value.getClass(), input), value);
    }

    public int getPositionBegin() {
        return positionBegin;
    }

    public Node setPositionBegin(int positionBegin) {
        this.positionBegin = positionBegin;
        return this;
    }

    public abstract String inspect();

    private void invalidTypeToMatchStringValue(Class<?> type, Object value1, Object value2, Operator.Matcher operator) {
        if (type.isInstance(value1) && value2 instanceof String)
            throw new RuntimeException(String.format("Cannot matches between type '%s' and 'java.lang.String'",
                    type.getName()), operator.getPosition());
    }

    private void shouldBeSameTypeIfTypeIs(Class<?> type, Object value1, Object value2, Operator.Matcher operator) {
        if (type.isInstance(value2) && value1 != null && !type.isInstance(value1))
            throw new RuntimeException(String.format("Cannot matches between type '%s' and '%s'",
                    getClassName(value1), type.getName()), operator.getPosition());
    }
}
