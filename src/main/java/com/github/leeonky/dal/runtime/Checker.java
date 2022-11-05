package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public interface Checker {
    Checker MATCH_NULL_CHECKER = CheckingContext::notationMatch;
    Checker DEFAULT_EQUALS_CHECKER = CheckingContext::notationEqualTo;
    Checker MATCH_CHECKER = new ConvertMatchChecker();

    default boolean failed(CheckingContext checkingContext) {
        return checkingContext.objectNotEquals();
    }

    String message(CheckingContext checkingContext);

    default Data transformActual(Data actual, Data expected, DALRuntimeContext context) {
        return actual;
    }

    default Data transformExpected(Data expected, DALRuntimeContext context) {
        return expected;
    }

    default boolean verify(CheckingContext checkingContext) {
        if (failed(checkingContext))
            throw new AssertionFailure(message(checkingContext), checkingContext.getPosition());
        return true;
    }

    class ConvertMatchChecker implements Checker {

        @Override
        public Data transformActual(Data actual, Data expected, DALRuntimeContext context) {
            return actual.convert(expected.getInstance().getClass());
        }

        @Override
        public String message(CheckingContext checkingContext) {
            return checkingContext.notationMatch();
        }
    }
}
