package com.github.leeonky.dal.parser;

import java.util.function.Predicate;

public abstract class SourceCodeMatcher {
    public static SourceCodeMatcher not(SourceCodeMatcher matcher) {
        return createSourceCodeMatcher(context -> !matcher.matches(context));
    }

    public static SourceCodeMatcher createSourceCodeMatcher(Predicate<TokenParser> predicate) {
        return new SourceCodeMatcher() {
            @Override
            boolean matches(TokenParser context) {
                return predicate.test(context);
            }
        };
    }

    abstract boolean matches(TokenParser context);

    public SourceCodeMatcher when(SourceCodeMatcher matcher) {
        return createSourceCodeMatcher(context -> matches(context) && matcher.matches(context));
    }

    public SourceCodeMatcher except(SourceCodeMatcher matcher) {
        return when(not(matcher));
    }
}
