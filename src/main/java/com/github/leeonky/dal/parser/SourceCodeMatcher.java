package com.github.leeonky.dal.parser;

import java.util.function.Predicate;

public abstract class SourceCodeMatcher {
    public static SourceCodeMatcher not(SourceCodeMatcher matcher) {
        return createSourceCodeMatcher(context -> !matcher.matches(context));
    }

    static SourceCodeMatcher createSourceCodeMatcher(Predicate<ParsingContext> predicate) {
        return new SourceCodeMatcher() {
            @Override
            boolean matches(ParsingContext context) {
                return predicate.test(context);
            }
        };
    }

    abstract boolean matches(ParsingContext context);

    public SourceCodeMatcher when(SourceCodeMatcher matcher) {
        return createSourceCodeMatcher(context -> matches(context) && matcher.matches(context));
    }

    public SourceCodeMatcher except(SourceCodeMatcher matcher) {
        return when(not(matcher));
    }
}
