package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.runtime.CheckingContext;
import com.github.leeonky.dal.runtime.ConditionalChecker;
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

    //    TODO refactor
    public boolean verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        Data expected = evaluateData(context);
        Data actual = actualNode.evaluateData(context);
        ConditionalChecker checker = context.fetchEqualsChecker(expected, actual);
        return checker.verify(new CheckingContext(expected, actual, checker.transformExpected(expected, context),
                checker.transformActual(actual, context), getPositionBegin()));
    }

    private CheckingContext createCheckingContext(DALNode actualNode, DALRuntimeContext context) {
        Data expected = evaluateData(context);
        Data actual = actualNode.evaluateData(context);
        return new CheckingContext(expected, actual, expected, actual, getPositionBegin());
    }

    public boolean verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        Data expected = evaluateData(context);
        Data actual = actualNode.evaluateData(context);
        CheckingContext checkingContext = createCheckingContext(actualNode, context);
        ConditionalChecker checker = context.fetchMatchesChecker(checkingContext);
        return checker.verify(new CheckingContext(expected, actual, checker.transformExpected(expected, context),
                checker.transformActual(actual, context), getPositionBegin()));
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
