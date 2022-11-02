package com.github.leeonky.dal.runtime;

import java.util.function.Function;
import java.util.function.Predicate;

//TODO rename to Checker
public interface ConditionalChecker {
    ConditionalChecker MATCH_NULL_CHECKER = conditionalChecker(CheckingContext::actualNotNull, CheckingContext::notationMatch),
            MATCH_NUMBER_CHECKER = conditionalChecker(CheckingContext::numberNotEquals, CheckingContext::notationNumberMatch),
            DEFAULT_EQUALS_CHECKER = conditionalChecker(CheckingContext::objectNotEquals, CheckingContext::notationEqualTo),
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

    default Data transformActual(Data actual) {
        return actual;
    }

    default Data transformExpected(Data expected) {
        return expected;
    }

    @Deprecated
    default ConditionalChecker and(ConditionalChecker another) {
        return new ConditionalChecker() {
            private String message;

            @Override
            public boolean failed(CheckingContext checkingContext) {
                boolean verified = ConditionalChecker.this.verify(checkingContext);
                if (!verified) {
                    message = ConditionalChecker.this.message(checkingContext);
                    return true;
                }
                verified = another.verify(checkingContext);
                if (!verified)
                    message = another.message(checkingContext);
                return !verified;
            }

            @Override
            public String message(CheckingContext checkingContext) {
                return message;
            }
        };
    }

    default boolean verify(CheckingContext checkingContext) {
        if (failed(checkingContext))
            throw new AssertionFailure(message(checkingContext), checkingContext.getPosition());
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
