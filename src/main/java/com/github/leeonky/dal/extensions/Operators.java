package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.Operation;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.NumberType;
import com.github.leeonky.util.function.TriFunction;

import static com.github.leeonky.dal.runtime.Operators.*;

public class Operators implements Extension {

    @Override
    public void extend(DAL dal) {
        numberCalculator(dal, PLUS, NumberType::plus);
        stringPlus(dal);

        numberCalculator(dal, SUB, NumberType::subtract);

        numberCalculator(dal, MUL, NumberType::multiply);

        numberCalculator(dal, DIV, NumberType::divide);
    }

    private void stringPlus(DAL dal) {
        dal.getRuntimeContextBuilder().registerOperator(PLUS, new Operation() {

            @Override
            public boolean match(Data v1, Data v2, DALRuntimeContext context) {
                return v1.instance() instanceof String || v2.instance() instanceof String;
            }

            @Override
            public Object operate(Data v1, Data v2, DALRuntimeContext context) {
                return String.valueOf(v1.instance()) + v2.instance();
            }
        });
    }

    private void numberCalculator(DAL dal, com.github.leeonky.dal.runtime.Operators operator,
                                  TriFunction<NumberType, Number, Number, Number> action) {
        dal.getRuntimeContextBuilder().registerOperator(operator, new Operation() {

            @Override
            public boolean match(Data v1, Data v2, DALRuntimeContext context) {
                return v1.instance() instanceof Number && v2.instance() instanceof Number;
            }

            @Override
            public Object operate(Data v1, Data v2, DALRuntimeContext context) {
                return action.apply(context.getNumberType(), (Number) v1.instance(), (Number) v2.instance());
            }
        });
    }
}
