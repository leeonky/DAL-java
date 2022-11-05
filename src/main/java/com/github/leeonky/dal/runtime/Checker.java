package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.Optional;
import java.util.function.Function;

public interface Checker {
    Checker MATCH_NULL_CHECKER = CheckingContext::messageMatch;
    Checker DEFAULT_EQUALS_CHECKER = CheckingContext::messageEqualTo;
    Checker MATCH_CHECKER = new ConvertMatchChecker();

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
            return checkingContext.messageMatch();
        }
    }

    static CheckerFactory factory(Checker checker) {
        return (d1, d2) -> Optional.of(checker);
    }
}
