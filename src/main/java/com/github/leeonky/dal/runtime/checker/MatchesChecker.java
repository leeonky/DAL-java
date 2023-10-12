package com.github.leeonky.dal.runtime.checker;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.math.BigDecimal;

public class MatchesChecker implements Checker {

    @Override
    public Data transformActual(Data actual, Data expected, RuntimeContextBuilder.DALRuntimeContext context) {
        return actual.convert(expected.getInstance().getClass());
    }

    @Override
    public String message(CheckingContext checkingContext) {
        return checkingContext.messageMatch();
    }

    @Override
    public boolean failed(CheckingContext checkingContext) {
        if (checkingContext.getExpected().getInstance() instanceof BigDecimal
                && checkingContext.getActual().getInstance() instanceof BigDecimal)
            return ((BigDecimal) checkingContext.getExpected().getInstance())
                    .compareTo((BigDecimal) checkingContext.getActual().getInstance()) != 0;
        return Checker.super.failed(checkingContext);
    }
}
