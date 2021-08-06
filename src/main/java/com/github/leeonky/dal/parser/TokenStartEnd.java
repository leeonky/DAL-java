package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.NoMoreSourceCodeException;

import static com.github.leeonky.dal.util.IfThen.when;

public abstract class TokenStartEnd {
    public static final TokenStartEnd END_OF_CODE = new TokenStartEnd() {
        @Override
        boolean matches(ParseContext context) {
            return !context.sourceCode.notEnd();
        }
    };

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

    public TokenStartEnd or(TokenStartEnd another) {
        return new TokenStartEnd() {
            @Override
            boolean matches(ParseContext context) {
                return TokenStartEnd.this.matches(context) || another.matches(context);
            }
        };
    }

    public TokenStartEnd orThrow(String message) {
        return new TokenStartEnd() {
            @Override
            boolean matches(ParseContext context) {
                try {
                    return TokenStartEnd.this.matches(context);
                } catch (NoMoreSourceCodeException ignore) {
                    throw new SyntaxException(context.sourceCode.getPosition(), message);
                }
            }
        };
    }
}
