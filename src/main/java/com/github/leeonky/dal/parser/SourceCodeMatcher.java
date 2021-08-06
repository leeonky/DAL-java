package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.Scanner;

import java.util.Objects;
import java.util.function.Predicate;

public abstract class SourceCodeMatcher {
    public static final SourceCodeMatcher DIGITAL = oneCharMatcher(Scanner.DIGITAL_CHAR::contains);
    public static final SourceCodeMatcher DELIMITER = oneCharMatcher(Scanner.TOKEN_DELIMITER::contains);
    public static final SourceCodeMatcher OPERATOR = oneCharMatcher(Scanner.OPERATOR_CHAR::contains);
    public static final SourceCodeMatcher AFTER_TOKEN_MATCHES = createSourceCodeMatcher(ParseContext::isLastTokenOperatorMatches);
    public static final SourceCodeMatcher AFTER_OPERATOR_MATCHES = createSourceCodeMatcher(ParseContext::isParsingCodeOperatorMatches);
    public static final SourceCodeMatcher ANY_CHARACTERS = createSourceCodeMatcher(context -> context.sourceCode.notEnd());

    public static SourceCodeMatcher CHARACTER(char a) {
        return oneCharMatcher(character -> Objects.equals(character, a));
    }

    public static SourceCodeMatcher not(SourceCodeMatcher matcher) {
        return createSourceCodeMatcher(context -> !matcher.matches(context));
    }

    private static SourceCodeMatcher oneCharMatcher(Predicate<Character> predicate) {
        return createSourceCodeMatcher(context -> predicate.test(context.sourceCode.currentChar()));
    }

    private static SourceCodeMatcher createSourceCodeMatcher(Predicate<ParseContext> predicate) {
        return new SourceCodeMatcher() {
            @Override
            boolean matches(ParseContext context) {
                return predicate.test(context);
            }
        };
    }

    abstract boolean matches(ParseContext context);

    public SourceCodeMatcher when(SourceCodeMatcher matcher) {
        return createSourceCodeMatcher(context -> matches(context) && matcher.matches(context));
    }

    public SourceCodeMatcher except(SourceCodeMatcher matcher) {
        return when(not(matcher));
    }
}
