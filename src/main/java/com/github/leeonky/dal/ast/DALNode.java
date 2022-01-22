package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.DALOperator.Matcher;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.NodeBase;

import static com.github.leeonky.dal.ast.AssertionFailure.*;
import static java.lang.String.format;

public abstract class DALNode extends NodeBase<RuntimeContextBuilder.DALRuntimeContext, DALNode> {

    public Data evaluateDataObject(RuntimeContextBuilder.DALRuntimeContext context) {
        return context.wrap(evaluate(context));
    }

    public boolean judge(DALNode actualNode, DALOperator.Equal operator, RuntimeContextBuilder.DALRuntimeContext context) {
        return assertEquals(evaluateDataObject(context), evaluateAndWrapperFailureMessage(actualNode, context),
                getPositionBegin());
    }

    public boolean judge(DALNode actualNode, Matcher operator, RuntimeContextBuilder.DALRuntimeContext context) {
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

    private Data evaluateAndWrapperFailureMessage(DALNode actualNode, RuntimeContextBuilder.DALRuntimeContext context) {
        try {
            return actualNode.evaluateDataObject(context);
        } catch (AssertionFailure assertionFailure) {
            throw assertionFailure.multiPosition(getPositionBegin(), Position.Type.CHAR);
        }
    }

    public abstract String inspect();

    private void invalidTypeToMatchValue(Class<?> actualType, Data actual, Class<?> expectedType, Data expected, Matcher operator) {
        if (actualType.isInstance(actual.getInstance()) && expectedType.isInstance(expected.getInstance()))
            throw new RuntimeException(format("Cannot compare between%sand%s", actual.inspect(), expected.inspect()),
                    operator.getPosition());
    }

    public DALNode avoidListMapping() {
        return this;
    }

    public Object getRootName() {
        return null;
    }

    public String inspectClause() {
        return inspect();
    }
}
