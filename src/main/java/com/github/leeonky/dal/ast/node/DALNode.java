package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ExpectActual;
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
        ExpectActual expectActual = new ExpectActual(evaluateData(context), actualNode.evaluateData(context), context);
        return context.fetchEqualsChecker(expectActual).verify(expectActual, getPositionBegin());
    }

    public boolean verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        ExpectActual expectActual = new ExpectActual(evaluateData(context), actualNode.evaluateData(context), context);
        return context.fetchMatchChecker(expectActual).verify(expectActual, getPositionBegin());
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
