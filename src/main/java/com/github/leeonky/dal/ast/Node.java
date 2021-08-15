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

    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        return Calculator.equals(actualNode.evaluate(context), evaluate(context));
    }

    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        Object expect = evaluate(context);
        Object actual = actualNode.evaluate(context);
        if (expect == null)
            return Objects.equals(actual, null);
        shouldBeSameTypeIfTypeIs(Number.class, actual, expect, operator);
        shouldBeSameTypeIfTypeIs(Boolean.class, actual, expect, operator);
        invalidTypeToMatchStringValue(Number.class, actual, expect, operator);
        invalidTypeToMatchStringValue(Boolean.class, actual, expect, operator);
        return Calculator.equals(context.getConverter().convert(expect.getClass(), actual), expect);
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
