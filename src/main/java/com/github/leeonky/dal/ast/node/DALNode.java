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

    public boolean verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        return assertEquals(evaluateData(context), actualNode.evaluateData(context), getPositionBegin());
    }

    public boolean verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        Data actual = actualNode.evaluateData(context);
        Data expected = evaluateData(context);
        if (expected.isNull())
            return assertMatchNull(actual, getPositionBegin());

        invalidTypeToMatchValue(String.class, actual, Number.class, expected);
        invalidTypeToMatchValue(String.class, actual, Boolean.class, expected);

        invalidTypeToMatchValue(Number.class, actual, String.class, expected);
        invalidTypeToMatchValue(Boolean.class, actual, String.class, expected);
        return assertMatch(expected, actual, getPositionBegin(), context.getNumberType());
    }

    public abstract String inspect();

    private void invalidTypeToMatchValue(Class<?> actualType, Data actual, Class<?> expectedType, Data expected) {
        if (actualType.isInstance(actual.getInstance()) && expectedType.isInstance(expected.getInstance()))
            throw new RuntimeException(format("Cannot compare between %sand %s", actual.inspect(), expected.inspect()).trim(),
                    getPositionBegin());
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
