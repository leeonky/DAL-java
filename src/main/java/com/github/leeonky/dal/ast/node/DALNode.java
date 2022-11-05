package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Factory;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.runtime.AssertionFailure;
import com.github.leeonky.dal.runtime.Checker;
import com.github.leeonky.dal.runtime.CheckingContext;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.NodeBase;

import java.util.List;
import java.util.stream.Stream;

public abstract class DALNode extends NodeBase<DALRuntimeContext, DALNode> {

    public Data evaluateData(DALRuntimeContext context) {
        return context.wrap(evaluate(context));
    }

    @Override
    public Object evaluate(DALRuntimeContext context) {
        return evaluateData(context).getInstance();
    }

    public boolean verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        return verify(actualNode, context, DALRuntimeContext::fetchEqualsChecker);
    }

    public boolean verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        return verify(actualNode, context, DALRuntimeContext::fetchMatchingChecker);
    }

    private boolean verify(DALNode actualNode, DALRuntimeContext context,
                           Factory.TriFunction<DALRuntimeContext, Data, Data, Checker> factory) {
        Data expected = evaluateData(context);
        Data actual = actualNode.evaluateData(context);
        Checker checker = factory.apply(context, expected, actual);
        return checker.verify(createContext(context, expected, actual, checker));
    }

    private CheckingContext createContext(DALRuntimeContext context, Data expected, Data actual, Checker checker) {
        Data transformedActual;
        try {
            transformedActual = checker.transformActual(actual, expected, context);
        } catch (Exception ex) {
            throw new AssertionFailure(ex.getMessage(), getPositionBegin());
        }
        return new CheckingContext(expected, actual, checker.transformExpected(expected, context),
                transformedActual, getPositionBegin());
    }

    public abstract String inspect();

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
