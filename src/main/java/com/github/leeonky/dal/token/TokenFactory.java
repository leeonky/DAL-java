package com.github.leeonky.dal.token;

import java.math.BigDecimal;
import java.util.Optional;

import static com.github.leeonky.dal.token.ContextMatches.*;
import static com.github.leeonky.dal.token.ContextPredicate.AFTER_OPERATOR_MATCHES;
import static com.github.leeonky.dal.token.ContextPredicate.AFTER_TOKEN_MATCHES;
import static com.github.leeonky.dal.token.EndWith.before;
import static com.github.leeonky.dal.token.Token.propertyToken;
import static java.util.Optional.empty;
import static java.util.Optional.of;

public interface TokenFactory {
    static TokenFactory createNumberTokenFactory() {
        return new TokenFactoryBase(
                StartWith.included(DIGITAL),
                Content.allChars(),
                EndWith.NO_MORE_SOURCE_CODE.or(before(TOKEN_DELIMITER))) {

            @Override
            protected Token createToken(String content) {
                return getNumber(content).map(Token::constValueToken).orElse(null);
            }

            private Optional<Number> getNumber(String content) {
                try {
                    return of(BigDecimal.valueOf(Long.decode(content)));
                } catch (NumberFormatException e) {
                    try {
                        return of(new BigDecimal(content));
                    } catch (Exception exception) {
                        return empty();
                    }
                }
            }
        };
    }

    static TokenFactory createBeanPropertyTokenFactory() {
        return new TokenFactoryBase(
                StartWith.excluded(CHARACTER('.')),
                //TODO to be refactor
                Content.leftTrim(Content.allChars()),
                EndWith.NO_MORE_SOURCE_CODE.or(before(TOKEN_DELIMITER))) {
            @Override
            protected Token createToken(String content) {
                if (content.isEmpty())
                    throw new IllegalTokenContentException("property chain not finished");
                return propertyToken(content.split("\\."));
            }
        };
    }

    static TokenFactory createOperatorTokenFactory() {
        return new TokenFactoryBase(
                StartWith.included(OPERATOR.except(CHARACTER('/').when(AFTER_TOKEN_MATCHES))),
                Content.allChars(),
                EndWith.NO_MORE_SOURCE_CODE
                        .or(before(not(OPERATOR)))
                        .or(before(CHARACTER('/').when(AFTER_OPERATOR_MATCHES)))) {

            @Override
            protected Token createToken(String content) {
                return Token.operatorToken(content);
            }
        };
    }

    static TokenFactory createSingleQuotedStringTokenFactory() {
        return new TokenFactoryBase(
                StartWith.excluded(CHARACTER('\'')),
                Content.allChars()
                        .escape("\\'", '\'')
                        .escape("\\\\", '\\'),
                EndWith.excluded(CHARACTER('\'')).orThrow("string should end with `'`")) {

            @Override
            protected Token createToken(String content) {
                return Token.constValueToken(content);
            }
        };
    }

    static TokenFactory createDoubleQuotedStringTokenFactory() {
        return new TokenFactoryBase(
                StartWith.excluded(CHARACTER('"')),
                Content.allChars()
                        .escape("\\\"", '"')
                        .escape("\\t", '\t')
                        .escape("\\n", '\n')
                        .escape("\\\\", '\\'),
                EndWith.excluded(CHARACTER('"')).orThrow("string should end with `\"`")) {

            @Override
            protected Token createToken(String content) {
                return Token.constValueToken(content);
            }
        };
    }

    static TokenFactory createRegexTokenFactory() {
        return new TokenFactoryBase(
                StartWith.excluded(CHARACTER('/').when(AFTER_TOKEN_MATCHES)),
                Content.allChars()
                        .escape("\\\\", '\\')
                        .escape("\\/", '/'),
                EndWith.excluded(CHARACTER('/')).orThrow("regex should end with `/`")) {

            @Override
            protected Token createToken(String content) {
                return Token.regexToken(content);
            }
        };
    }

    Token fetchToken(SourceCode sourceCode, Token previous);
}
