package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Match;
import com.github.leeonky.dal.runtime.AssertionFailure;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.checker.Checker;
import com.github.leeonky.dal.runtime.checker.CheckingContext;
import com.github.leeonky.interpreter.NodeBase;
import com.github.leeonky.util.function.TriFunction;

import java.util.List;
import java.util.stream.Stream;

public abstract class DALNode extends NodeBase<DALRuntimeContext, DALNode> {

    public Data evaluateData(DALRuntimeContext context) {
        return context.wrap(evaluate(context));
    }

    @Override
    public Object evaluate(DALRuntimeContext context) {
        return evaluateData(context).instance();
    }

    @Deprecated
    public Data verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        return verify(actualNode, context, DALRuntimeContext::fetchEqualsChecker);
    }

    @Deprecated
    public Data verify(DALNode actualNode, Match operator, DALRuntimeContext context) {
        return verify(actualNode, context, DALRuntimeContext::fetchMatchingChecker);
    }

    @Deprecated
    private Data verify(DALNode actualNode, DALRuntimeContext context,
                        TriFunction<DALRuntimeContext, Data, Data, Checker> factory) {
        Data expected = evaluateData(context);
        expected.isNullWithPosition(getOperandPosition());
        Data actual = actualNode.evaluateData(context);
        actual.isNullWithPosition(actualNode.getOperandPosition());
        return checkerVerify(factory.apply(context, expected, actual), expected, actual, context);
    }

    @Deprecated
    protected Data checkerVerify(Checker checker, Data expected, Data actual, DALRuntimeContext context) {
        return checker.verify(createContext(context, expected, actual, checker));
    }

    @Deprecated
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
