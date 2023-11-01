package com.github.leeonky.dal.runtime.checker;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.math.BigDecimal;

public class MatchesChecker implements Checker {

    @Override
    public Data transformActual(Data actual, Data expected, RuntimeContextBuilder.DALRuntimeContext context) {
        return actual.convert(expected.instance().getClass());
    }

    @Override
    public String message(CheckingContext checkingContext) {
        return checkingContext.messageMatch();
    }

    @Override
    public boolean failed(CheckingContext checkingContext) {
        if (checkingContext.getExpected().instance() instanceof BigDecimal
                && checkingContext.getActual().instance() instanceof BigDecimal)
            return ((BigDecimal) checkingContext.getExpected().instance())
                    .compareTo((BigDecimal) checkingContext.getActual().instance()) != 0;
        return Checker.super.failed(checkingContext);
    }
}
