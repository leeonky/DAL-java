package com.github.leeonky.dal.runtime;

import java.util.function.Function;
import java.util.function.Predicate;

public interface ConditionalChecker extends Checker {
    Checker MATCH_NULL_CHECKER = conditionalChecker(CheckingContext::actualNotNull, CheckingContext::notationMatch),
            MATCH_NUMBER_CHECKER = conditionalChecker(CheckingContext::numberNotEquals, CheckingContext::notationNumberMatch),
            EQUALS_CHECKER = conditionalChecker(CheckingContext::objectNotEquals, CheckingContext::notationEqualTo),
            MATCH_CHECKER = matchTypeChecker(String.class, Number.class)
                    .and(matchTypeChecker(String.class, Boolean.class))
                    .and(matchTypeChecker(Number.class, String.class))
                    .and(matchTypeChecker(Boolean.class, String.class))
                    .and(new ConvertMatchChecker());

    static ConditionalChecker matchTypeChecker(Class<?> actualType, Class<?> expectType) {
        return conditionalChecker(expectActual -> expectActual.isInstanceOf(actualType, expectType),
                CheckingContext::cannotCompare);
    }

    static ConditionalChecker conditionalChecker(Predicate<CheckingContext> failed, Function<CheckingContext, String> message) {
        return new ConditionalChecker() {
            @Override
            public String message(CheckingContext checkingContext) {
                return message.apply(checkingContext);
            }

            @Override
            public boolean failed(CheckingContext checkingContext) {
                return failed.test(checkingContext);
            }
        };
    }

    boolean failed(CheckingContext checkingContext);

    String message(CheckingContext checkingContext);

    @Override
    default boolean verify(CheckingContext checkingContext, int position) {
        if (failed(checkingContext))
            throw new AssertionFailure(message(checkingContext), position);
        return true;
    }

    class ConvertMatchChecker implements ConditionalChecker {
        private String message;

        @Override
        public boolean failed(CheckingContext checkingContext) {
            try {
                Data result = checkingContext.convertToExpectedType();
                if (checkingContext.equalTo(result))
                    return false;
                message = checkingContext.notationMatch(result);
            } catch (Exception e) {
                message = e.getMessage();
            }
            return true;
        }

        @Override
        public String message(CheckingContext checkingContext1) {
            return message;
        }
    }
}
