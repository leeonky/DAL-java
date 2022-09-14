package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.interpreter.NodeBase;

import java.util.List;
import java.util.stream.Stream;

import static com.github.leeonky.dal.runtime.AssertionFailure.*;
import static java.lang.String.format;

public abstract class DALNode extends NodeBase<DALRuntimeContext, DALNode> {

    public Data evaluateData(DALRuntimeContext context) {
        return context.wrap(evaluate(context));
    }

    @Override
    public Object evaluate(DALRuntimeContext context) {
        return evaluateData(context).getInstance();
    }

    public boolean verifyBy(DALNode expected, Equal operator, DALRuntimeContext context) {
        return expected.verify(this, operator, context);
    }

    public boolean verifyBy(DALNode expected, Matcher operator, DALRuntimeContext context) {
        return expected.verify(this, operator, context);
    }

    public boolean verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        return verify(actualNode.evaluateData(context), operator, context, actualNode);
    }

    public boolean verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        return verify(actualNode.evaluateData(context), operator, context, actualNode);
    }

    protected boolean verify(Data actual, Equal operator, DALRuntimeContext context, DALNode actualNode) {
        return assertEquals(evaluateData(context), actual, getPositionBegin());
    }

    protected boolean verify(Data actual, Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        Data expected = evaluateData(context);
        if (expected.isNull())
            return assertMatchNull(actual, actualNode.getPositionBegin());

        invalidTypeToMatchValue(String.class, actual, Number.class, expected, operator);
        invalidTypeToMatchValue(String.class, actual, Boolean.class, expected, operator);

        invalidTypeToMatchValue(Number.class, actual, String.class, expected, operator);
        invalidTypeToMatchValue(Boolean.class, actual, String.class, expected, operator);
        return assertMatch(expected, actual, getPositionBegin(), context.getNumberType());
    }

    public abstract String inspect();

    private void invalidTypeToMatchValue(Class<?> actualType, Data actual, Class<?> expectedType, Data expected,
                                         Matcher operator) {
        if (actualType.isInstance(actual.getInstance()) && expectedType.isInstance(expected.getInstance()))
            throw new RuntimeException(format("Cannot compare between %sand %s", actual.inspect(), expected.inspect()).trim(),
                    operator.getPosition());
    }

    public Object getRootSymbolName() {
        return null;
    }

    public List<Object> propertyChain() {
        throw new IllegalStateException();
    }

    public Stream<Object> collectFields(Data data) {
        return Stream.of(data.firstFieldFromAlias(getRootSymbolName()));
    }
}
