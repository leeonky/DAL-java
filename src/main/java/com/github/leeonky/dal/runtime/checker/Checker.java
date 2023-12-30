package com.github.leeonky.dal.runtime.checker;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.function.Function;

public interface Checker {
    Checker MATCH_NULL_CHECKER = CheckingContext::messageMatch;
    Checker EQUALS_CHECKER = new EqualsChecker();
    Checker MATCHES_CHECKER = new MatchesChecker();

    static Checker forceFailed(Function<CheckingContext, String> message) {
        return new Checker() {
            @Override
            public boolean failed(CheckingContext checkingContext) {
                return true;
            }

            @Override
            public String message(CheckingContext checkingContext) {
                return message.apply(checkingContext);
            }
        };
    }

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

    default Data verify(CheckingContext checkingContext) {
        if (failed(checkingContext))
            throw new AssertionError(message(checkingContext));
        return checkingContext.getOriginalActual();
    }
}
