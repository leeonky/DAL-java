package com.github.leeonky.dal.runtime;

import java.util.function.Function;
import java.util.function.Predicate;

import static java.lang.String.format;

public interface ConditionalChecker extends Checker {
    Checker MATCH_NULL_CHECKER = conditionalChecker(ExpectActual::actualNotNull, ExpectActual::shouldMatchNull),
            MATCH_NUMBER_CHECKER = conditionalChecker(ExpectActual::numberNotEquals, ExpectActual::shouldMatch),
            EQUALS_CHECKER = conditionalChecker(ExpectActual::objectNotEquals, ExpectActual::shouldEqualTo),
            MATCH_CHECKER = matchTypeChecker(String.class, Number.class)
                    .and(matchTypeChecker(String.class, Boolean.class))
                    .and(matchTypeChecker(Number.class, String.class))
                    .and(matchTypeChecker(Boolean.class, String.class))
                    .and(new ConvertMatchChecker());

    static ConditionalChecker matchTypeChecker(Class<?> actualType, Class<?> expectType) {
        return new ConditionalChecker() {
            @Override
            public boolean failed(ExpectActual expectActual) {
                return expectActual.isInstanceOf(actualType, expectType);
            }

            @Override
            public String message(ExpectActual expectActual) {
                return format("Cannot compare between %sand %s", expectActual.getActual().inspect(), expectActual.getExpected().inspect());
            }
        };
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
                if (Calculator.equals(result, expectActual.getExpected())) {
                    return false;
                }
                message = expectActual.shouldMatch(result);
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
