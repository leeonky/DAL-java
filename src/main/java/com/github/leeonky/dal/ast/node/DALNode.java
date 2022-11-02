package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.runtime.CheckerFactory;
import com.github.leeonky.dal.runtime.CheckingContext;
import com.github.leeonky.dal.runtime.ConditionalChecker;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.NodeBase;

import java.util.List;
import java.util.Optional;
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
        CheckingContext checkingContext = createCheckingContext(actualNode, context);
        Optional<CheckerFactory> checkerFactory = context.fetchEqualsChecker(checkingContext.getExpected(), checkingContext.getActual());
        if (checkerFactory.isPresent()) {
            CheckerFactory factory = checkerFactory.get();
            Optional<ConditionalChecker> conditionalChecker = factory.create(checkingContext.getExpected(), checkingContext.getActual());
            if (conditionalChecker.isPresent()) {
                return conditionalChecker.get().verify(
                        new CheckingContext(checkingContext.getExpected(), checkingContext.getActual(),
                                factory.transformExpected(checkingContext.getExpected()),
                                factory.transformActual(checkingContext.getActual()), getPositionBegin()));
            }
        }
        return checkerFactory.flatMap(factory -> factory.create(checkingContext.getExpected(), checkingContext.getActual()))
                .orElseGet(() -> context.fetchEqualsChecker(checkingContext)).verify(checkingContext);
//        return context.fetchEqualsChecker(checkingContext)
    }

    private CheckingContext createCheckingContext(DALNode actualNode, DALRuntimeContext context) {
        Data expected = evaluateData(context);
        Data actual = actualNode.evaluateData(context);
        return new CheckingContext(expected, actual, expected, actual, getPositionBegin());
    }

    public boolean verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        CheckingContext checkingContext = createCheckingContext(actualNode, context);
        return context.fetchMatchesChecker(checkingContext).verify(checkingContext);
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
