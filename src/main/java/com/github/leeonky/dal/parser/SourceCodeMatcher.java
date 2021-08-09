package com.github.leeonky.dal.parser;

import java.util.function.Predicate;

public abstract class SourceCodeMatcher {
    public static SourceCodeMatcher not(SourceCodeMatcher matcher) {
        return createSourceCodeMatcher(parser -> !matcher.matches(parser));
    }

    public static SourceCodeMatcher createSourceCodeMatcher(Predicate<TokenParser> predicate) {
        return new SourceCodeMatcher() {
            @Override
            boolean matches(TokenParser parser) {
                return predicate.test(parser);
            }
        };
    }

    abstract boolean matches(TokenParser parser);

    public SourceCodeMatcher when(SourceCodeMatcher matcher) {
        return createSourceCodeMatcher(parser -> matches(parser) && matcher.matches(parser));
    }

    public SourceCodeMatcher except(SourceCodeMatcher matcher) {
        return when(not(matcher));
    }
}
