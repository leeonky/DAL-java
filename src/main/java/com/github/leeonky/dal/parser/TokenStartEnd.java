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
            boolean matches(TokenParser parser) {
                return predicate.test(parser);
            }
        };
    }

    abstract boolean matches(TokenParser parser);

    public TokenStartEnd or(TokenStartEnd another) {
        return createTokenStartEnd(parser -> matches(parser) || another.matches(parser));
    }

    public TokenStartEnd orThrow(String message) {
        return createTokenStartEnd(parser -> {
            try {
                return matches(parser);
            } catch (NoMoreSourceCodeException exception) {
                throw new SyntaxException(message, exception.getPosition());
            }
        });
    }
}
