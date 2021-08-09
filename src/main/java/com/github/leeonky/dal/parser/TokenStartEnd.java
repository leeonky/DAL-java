package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.NoMoreSourceCodeException;

import java.util.function.Predicate;

public abstract class TokenStartEnd {
    public static TokenStartEnd before(SourceCodeMatcher sourceCodeMatcher) {
        return createTokenStartEnd(sourceCodeMatcher::matches);
    }

    static TokenStartEnd createTokenStartEnd(Predicate<TokenParser> predicate) {
        return new TokenStartEnd() {
            @Override
            boolean matches(TokenParser context) {
                return predicate.test(context);
            }
        };
    }

    abstract boolean matches(TokenParser context);

    public TokenStartEnd or(TokenStartEnd another) {
        return createTokenStartEnd(context -> matches(context) || another.matches(context));
    }

    public TokenStartEnd orThrow(String message) {
        return createTokenStartEnd(context -> {
            try {
                return matches(context);
            } catch (NoMoreSourceCodeException exception) {
                throw new SyntaxException(exception.getPosition(), message);
            }
        });
    }
}
