package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.Operator.Matcher;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import static com.github.leeonky.dal.ast.AssertionFailure.*;
import static java.lang.String.format;

public abstract class Node {
    private int positionBegin;

    public Object evaluate(RuntimeContextBuilder.RuntimeContext context) {
        throw new IllegalStateException();
    }

    public Data evaluateDataObject(RuntimeContextBuilder.RuntimeContext context) {
        return context.wrap(evaluate(context));
    }

    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContextBuilder.RuntimeContext context) {
        return assertEquals(evaluateDataObject(context), evaluateAndWrapperFailureMessage(actualNode, context),
                getPositionBegin());
    }

    public boolean judge(Node actualNode, Matcher operator, RuntimeContextBuilder.RuntimeContext context) {
        Data expected = evaluateDataObject(context);
        Data actual = evaluateAndWrapperFailureMessage(actualNode, context);
        if (expected.isNull())
            return assertMatchNull(actual, actualNode.getPositionBegin());

        invalidTypeToMatchValue(String.class, actual, Number.class, expected, operator);
        invalidTypeToMatchValue(String.class, actual, Boolean.class, expected, operator);

        invalidTypeToMatchValue(Number.class, actual, String.class, expected, operator);
        invalidTypeToMatchValue(Boolean.class, actual, String.class, expected, operator);
        return assertMatch(expected, actual, getPositionBegin(), context.getConverter());
    }

    private Data evaluateAndWrapperFailureMessage(Node actualNode, RuntimeContextBuilder.RuntimeContext context) {
        try {
            return actualNode.evaluateDataObject(context);
        } catch (AssertionFailure assertionFailure) {
            throw assertionFailure.multiPosition(getPositionBegin(), Position.Type.CHAR);
        }
    }

    public int getPositionBegin() {
        return positionBegin;
    }

    public Node setPositionBegin(int positionBegin) {
        this.positionBegin = positionBegin;
        return this;
    }

    public abstract String inspect();

    private void invalidTypeToMatchValue(Class<?> actualType, Data actual, Class<?> expectedType, Data expected, Matcher operator) {
        if (actualType.isInstance(actual.getInstance()) && expectedType.isInstance(expected.getInstance()))
            throw new RuntimeException(format("Cannot compare between%sand%s", actual.inspect(), expected.inspect()),
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

    public int getOperandPosition() {
        return positionBegin;
    }
}
