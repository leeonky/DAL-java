package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.node.table.RowHeader;
import com.github.leeonky.dal.ast.node.table.RowType;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ExpectationFactory;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.checker.Checker;
import com.github.leeonky.dal.runtime.checker.CheckingContext;
import com.github.leeonky.interpreter.NodeBase;

import java.util.List;
import java.util.stream.Stream;

import static com.github.leeonky.dal.runtime.ExpressionException.opt1;
import static com.github.leeonky.dal.runtime.ExpressionException.opt2;

public abstract class DALNode extends NodeBase<DALRuntimeContext, DALNode> {

    public Data evaluateData(DALRuntimeContext context) {
        return context.wrap(evaluate(context));
    }

    @Override
    public Object evaluate(DALRuntimeContext context) {
        return evaluateData(context).instance();
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

    public Data verify(DALOperator operator, DALNode actual, DALRuntimeContext context) {
        return context.calculate(actual.evaluateData(context), operator, context.wrap(toVerify(context)));
    }

    protected ExpectationFactory toVerify(DALRuntimeContext context) {
        Data expected = evaluateData(context);
        return (operator, actual) -> new ExpectationFactory.Expectation() {
            @Override
            public Data matches() {
                Checker checker = context.fetchMatchingChecker(expected, actual);
                return checker.verify(new CheckingContext(expected, actual,
                        opt2(() -> checker.transformExpected(expected, context)),
                        opt1(() -> checker.transformActual(actual, expected, context))));
            }

            @Override
            public Data equalTo() {
                Checker checker = context.fetchEqualsChecker(expected, actual);
                return checker.verify(new CheckingContext(expected, actual,
                        opt2(() -> checker.transformExpected(expected, context)),
                        opt1(() -> checker.transformActual(actual, expected, context))));
            }

            @Override
            public ExpectationFactory.Type type() {
                return ExpectationFactory.Type.VALUE;
            }
        };
    }

    public RowType guessTableHeaderType() {
        return RowHeader.DEFAULT_INDEX;
    }

    public boolean needPrefixBlankWarningCheck() {
        return false;
    }

    public boolean needPostBlankWarningCheck() {
        return false;
    }
}
