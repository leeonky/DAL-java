package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.DataObject;
import com.github.leeonky.dal.runtime.RuntimeContext;

import static com.github.leeonky.dal.ast.AssertionFailure.*;
import static com.github.leeonky.util.BeanClass.getClassName;
import static java.lang.String.format;

public abstract class Node {
    private int positionBegin;

    public Object evaluate(RuntimeContext context) {
        throw new IllegalStateException();
    }

    public DataObject evaluateDataObject(RuntimeContext context) {
        return context.wrap(evaluate(context));
    }

    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        return assertEquals(evaluate(context), actualNode.evaluate(context), getPositionBegin());
    }

    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        Object expect = evaluate(context);
        Object actual = actualNode.evaluate(context);
        if (expect == null)
            return assertMatchNull(actual, actualNode.getPositionBegin());
        shouldBeSameTypeIfTypeIs(Number.class, actual, expect, operator);
        shouldBeSameTypeIfTypeIs(Boolean.class, actual, expect, operator);
        invalidTypeToMatchStringValue(Number.class, actual, expect, operator);
        invalidTypeToMatchStringValue(Boolean.class, actual, expect, operator);
        return assertMatch(expect, actual, getPositionBegin(), context.getConverter());
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

    public Node avoidListMapping() {
        return this;
    }
}
