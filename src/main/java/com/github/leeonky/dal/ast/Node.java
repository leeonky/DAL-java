package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.Operator.Matcher;
import com.github.leeonky.dal.runtime.DataObject;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import static com.github.leeonky.dal.ast.AssertionFailure.*;
import static java.lang.String.format;

public abstract class Node {
    private int positionBegin;

    public Object evaluate(RuntimeContextBuilder.RuntimeContext context) {
        throw new IllegalStateException();
    }

    public DataObject evaluateDataObject(RuntimeContextBuilder.RuntimeContext context) {
        return context.wrap(evaluate(context));
    }

    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContextBuilder.RuntimeContext context) {
        return assertEquals(evaluateDataObject(context), actualNode.evaluateDataObject(context), getPositionBegin());
    }

    public boolean judge(Node actualNode, Matcher operator, RuntimeContextBuilder.RuntimeContext context) {
        DataObject expected = evaluateDataObject(context);
        DataObject actual = actualNode.evaluateDataObject(context);
        if (expected.isNull())
            return assertMatchNull(actual, actualNode.getPositionBegin());
        shouldBeSameTypeIfTypeIs(Number.class, actual, operator, expected);
        shouldBeSameTypeIfTypeIs(Boolean.class, actual, operator, expected);
        invalidTypeToMatchStringValue(Number.class, actual, expected, operator);
        invalidTypeToMatchStringValue(Boolean.class, actual, expected, operator);
        return assertMatch(expected, actual, getPositionBegin(), context.getConverter());
    }

    public int getPositionBegin() {
        return positionBegin;
    }

    public Node setPositionBegin(int positionBegin) {
        this.positionBegin = positionBegin;
        return this;
    }

    public abstract String inspect();

    private void invalidTypeToMatchStringValue(Class<?> type, DataObject actual, DataObject expected, Matcher operator) {
        if (type.isInstance(actual.getInstance()) && expected.getInstance() instanceof String)
            throw new RuntimeException(format("Cannot compare between%sand 'java.lang.String'", actual.inspect()),
                    operator.getPosition());
    }

    private void shouldBeSameTypeIfTypeIs(Class<?> type, DataObject value1, Matcher operator, DataObject value2) {
        if (type.isInstance(value2.getInstance()) && !value1.isNull() && !type.isInstance(value1.getInstance()))
            throw new RuntimeException(format("Cannot compare between%sand%s", value1.inspect(), value2.inspect()),
                    operator.getPosition());
    }

    public Node avoidListMapping() {
        return this;
    }

    public Object getRootName() {
        return null;
    }

    public String inspectClause() {
        return inspect();
    }
}
