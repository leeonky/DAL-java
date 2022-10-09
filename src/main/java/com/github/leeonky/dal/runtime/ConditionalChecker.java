package com.github.leeonky.dal.runtime;

import java.util.function.Function;
import java.util.function.Predicate;

public interface ConditionalChecker extends Checker {
    Checker MATCH_NULL_CHECKER = conditionalChecker(ExpectActual::actualNotNull, ExpectActual::notationMatch),
            MATCH_NUMBER_CHECKER = conditionalChecker(ExpectActual::numberNotEquals, ExpectActual::notationMatch),
            EQUALS_CHECKER = conditionalChecker(ExpectActual::objectNotEquals, ExpectActual::notationEqualTo),
            MATCH_CHECKER = matchTypeChecker(String.class, Number.class)
                    .and(matchTypeChecker(String.class, Boolean.class))
                    .and(matchTypeChecker(Number.class, String.class))
                    .and(matchTypeChecker(Boolean.class, String.class))
                    .and(new ConvertMatchChecker());

    static ConditionalChecker matchTypeChecker(Class<?> actualType, Class<?> expectType) {
        return conditionalChecker(expectActual -> expectActual.isInstanceOf(actualType, expectType),
                ExpectActual::cannotCompare);
    }

    static ConditionalChecker conditionalChecker(Predicate<ExpectActual> failed, Function<ExpectActual, String> message) {
        return new ConditionalChecker() {
            @Override
            public String message(ExpectActual expectActual) {
                return message.apply(expectActual);
            }

            @Override
            public boolean failed(ExpectActual expectActual) {
                return failed.test(expectActual);
            }
        };
    }

    boolean failed(ExpectActual expectActual);

    String message(ExpectActual expectActual);

    @Override
    default boolean verify(ExpectActual expectActual, int position) {
        if (failed(expectActual))
            throw new AssertionFailure(message(expectActual), position);
        return true;
    }

    class ConvertMatchChecker implements ConditionalChecker {
        private String message;

        @Override
        public boolean failed(ExpectActual expectActual) {
            try {
                Data result = expectActual.convertToExpectedType();
                if (expectActual.equalTo(result))
                    return false;
                message = expectActual.notationMatch(result);
            } catch (Exception e) {
                message = e.getMessage();
            }
            return true;
        }

        @Override
        public String message(ExpectActual expectActual1) {
            return message;
        }
    }
}
