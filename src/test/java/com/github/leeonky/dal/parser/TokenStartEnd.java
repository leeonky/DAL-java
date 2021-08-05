package com.github.leeonky.dal.parser;

import static com.github.leeonky.dal.util.IfThen.when;

public abstract class TokenStartEnd {
    public static TokenStartEnd included(SourceCodeMatcher sourceCodeMatcher) {
        return new TokenStartEnd() {

            @Override
            boolean matches(ParseContext context) {
                return when(sourceCodeMatcher.matches(context))
                        .then(() -> context.content.add(context.sourceCode.takeCurrentChar()));
            }
        };
    }

    public static TokenStartEnd excluded(SourceCodeMatcher sourceCodeMatcher) {
        return new TokenStartEnd() {

            @Override
            boolean matches(ParseContext context) {
                return when(sourceCodeMatcher.matches(context)).then(context.sourceCode::takeCurrentChar);
            }
        };
    }

    public static TokenStartEnd before(SourceCodeMatcher sourceCodeMatcher) {
        return new TokenStartEnd() {

            @Override
            boolean matches(ParseContext context) {
                return sourceCodeMatcher.matches(context);
            }
        };
    }

    abstract boolean matches(ParseContext context);
}
