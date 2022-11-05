package com.github.leeonky.dal.runtime.checker;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class MatchesChecker implements Checker {

    @Override
    public Data transformActual(Data actual, Data expected, RuntimeContextBuilder.DALRuntimeContext context) {
        return actual.convert(expected.getInstance().getClass());
    }

    @Override
    public String message(CheckingContext checkingContext) {
        return checkingContext.messageMatch();
    }
}
