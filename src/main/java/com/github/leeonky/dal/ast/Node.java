package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.AssertionFailure;
import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.util.Calculator;

import static com.github.leeonky.dal.ast.ConstNode.inspectValue;
import static com.github.leeonky.util.BeanClass.getClassName;
import static java.lang.String.format;

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
        if (expect == null) {
            if (actual != null)
                throw new AssertionFailure(format("[%s] does not match null", inspectValue(actual)),
                        actualNode.getPositionBegin());
            else
                return true;
        }
        shouldBeSameTypeIfTypeIs(Number.class, actual, expect, operator);
        shouldBeSameTypeIfTypeIs(Boolean.class, actual, expect, operator);
        invalidTypeToMatchStringValue(Number.class, actual, expect, operator);
        invalidTypeToMatchStringValue(Boolean.class, actual, expect, operator);
        if (!Calculator.equals(context.getConverter().convert(expect.getClass(), actual), expect))
            throw new AssertionFailure(format("expected [%s] matches [%s] but was not",
                    inspectValue(actual), inspectValue(expect)), actualNode.getPositionBegin());
        return true;
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
            throw new RuntimeException(format("Cannot matches between type '%s' and 'java.lang.String'",
                    type.getName()), operator.getPosition());
    }

    private void shouldBeSameTypeIfTypeIs(Class<?> type, Object value1, Object value2, Operator.Matcher operator) {
        if (type.isInstance(value2) && value1 != null && !type.isInstance(value1))
            throw new RuntimeException(format("Cannot matches between type '%s' and '%s'",
                    getClassName(value1), type.getName()), operator.getPosition());
    }
}
