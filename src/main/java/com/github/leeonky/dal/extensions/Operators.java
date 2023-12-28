package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ExpectationFactory;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.Operation;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.checker.Checker;
import com.github.leeonky.dal.runtime.checker.CheckingContext;
import com.github.leeonky.util.NumberType;
import com.github.leeonky.util.function.TriFunction;

import static com.github.leeonky.dal.runtime.ExpressionException.opt1;
import static com.github.leeonky.dal.runtime.ExpressionException.opt2;
import static com.github.leeonky.dal.runtime.Operators.*;

public class Operators implements Extension {

    @Override
    public void extend(DAL dal) {
        numberCalculator(dal, PLUS, NumberType::plus);
        stringPlus(dal);
        numberCalculator(dal, SUB, NumberType::subtract);
        numberCalculator(dal, MUL, NumberType::multiply);
        numberCalculator(dal, DIV, NumberType::divide);

        defaultEqual(dal);
        assertEqual(dal);
        defaultMatch(dal);
        assertMatch(dal);
    }

    private void assertMatch(DAL dal) {
        dal.getRuntimeContextBuilder().registerOperator(MATCH, new Operation() {

            @Override
            public boolean match(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                return v2.instance() instanceof ExpectationFactory;
            }

            @Override
            public Data operate(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                return ((ExpectationFactory) v2.instance()).create(operator, v1).matches();
            }
        });
    }

    private void assertEqual(DAL dal) {
        dal.getRuntimeContextBuilder().registerOperator(EQUAL, new Operation() {

            @Override
            public boolean match(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                return v2.instance() instanceof ExpectationFactory;
            }

            @Override
            public Data operate(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                return ((ExpectationFactory) v2.instance()).create(operator, v1).equalTo();
            }
        });
    }

    private void defaultMatch(DAL dal) {
        dal.getRuntimeContextBuilder().registerOperator(MATCH, new Operation() {

            @Override
            public boolean match(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                return true;
            }

            @Override
            public Data operate(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                Checker checker = context.fetchMatchingChecker(v2, v1);
                return checker.verify(new CheckingContext(v2, v1,
                        opt2(() -> checker.transformExpected(v2, context)),
                        opt1(() -> checker.transformActual(v1, v2, context)), -1));
            }
        });
    }

    private void defaultEqual(DAL dal) {
        dal.getRuntimeContextBuilder().registerOperator(EQUAL, new Operation() {

            @Override
            public boolean match(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                return true;
            }

            @Override
            public Data operate(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                Checker checker = context.fetchEqualsChecker(v2, v1);
                return checker.verify(new CheckingContext(v2, v1,
                        opt2(() -> checker.transformExpected(v2, context)),
                        opt1(() -> checker.transformActual(v1, v2, context)), -1));
            }
        });
    }

    private void stringPlus(DAL dal) {
        dal.getRuntimeContextBuilder().registerOperator(PLUS, new Operation() {

            @Override
            public boolean match(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                return v1.instance() instanceof String || v2.instance() instanceof String;
            }

            @Override
            public Data operate(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                return context.wrap(String.valueOf(v1.instance()) + v2.instance());
            }
        });
    }

    private void numberCalculator(DAL dal, com.github.leeonky.dal.runtime.Operators operator,
                                  TriFunction<NumberType, Number, Number, Number> action) {
        dal.getRuntimeContextBuilder().registerOperator(operator, new Operation() {

            @Override
            public boolean match(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                return v1.instance() instanceof Number && v2.instance() instanceof Number;
            }

            @Override
            public Data operate(Data v1, DALOperator operator, Data v2, DALRuntimeContext context) {
                return context.wrap(action.apply(context.getNumberType(), (Number) v1.instance(),
                        (Number) v2.instance()));
            }
        });
    }
}
