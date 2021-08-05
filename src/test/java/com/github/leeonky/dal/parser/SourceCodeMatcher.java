package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.Scanner;

import java.util.Objects;
import java.util.function.Predicate;

public abstract class SourceCodeMatcher {
    public static final SourceCodeMatcher DIGITAL = oneCharMatcher(Scanner.DIGITAL_CHAR::contains);
    public static final SourceCodeMatcher DELIMITER = oneCharMatcher(Scanner.TOKEN_DELIMITER::contains);
    public static final SourceCodeMatcher OPERATOR = oneCharMatcher(Scanner.OPERATOR_CHAR::contains);
    public static final SourceCodeMatcher AFTER_TOKEN_MATCHES = new SourceCodeMatcher() {
        @Override
        boolean matches(ParseContext context) {
            return context.isLastTokenOperatorMatches();
        }
    };
    public static final SourceCodeMatcher AFTER_OPERATOR_MATCHES = new SourceCodeMatcher() {
        @Override
        boolean matches(ParseContext context) {
            return context.content.size() == 1 && Scanner.OPT_MATCHES == context.content.get(0);
        }
    };

    private static SourceCodeMatcher oneCharMatcher(Predicate<Character> predicate) {
        return new SourceCodeMatcher() {
            @Override
            public boolean matches(ParseContext context) {
                return predicate.test(context.sourceCode.currentChar());
            }
        };
    }

    public static SourceCodeMatcher CHARACTER(char a) {
        return oneCharMatcher(character -> Objects.equals(character, a));
    }

    public static SourceCodeMatcher not(SourceCodeMatcher matcher) {
        return new SourceCodeMatcher() {
            @Override
            boolean matches(ParseContext context) {
                return !matcher.matches(context);
            }
        };
    }

    abstract boolean matches(ParseContext context);

    public SourceCodeMatcher when(SourceCodeMatcher matcher) {
        return new SourceCodeMatcher() {
            @Override
            boolean matches(ParseContext context) {
                return SourceCodeMatcher.this.matches(context) && matcher.matches(context);
            }
        };
    }

    public SourceCodeMatcher except(SourceCodeMatcher matcher) {
        return when(not(matcher));
    }
}
