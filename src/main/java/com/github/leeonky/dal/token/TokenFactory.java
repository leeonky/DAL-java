package com.github.leeonky.dal.token;

import java.math.BigDecimal;
import java.util.Optional;

public interface TokenFactory {
    static TokenFactory createNumberTokenFactory() {
        return new TokenFactoryBase(
                StartWith.included(ContextMatches.DIGITAL),
                Content.allChars(),
                EndWith.NO_MORE_SOURCE_CODE.or(EndWith.before(ContextMatches.TOKEN_DELIMITER))) {

            @Override
            protected Token createToken(String content) {
                return getNumber(content).map(Token::constValueToken).orElse(null);
            }

            private Optional<Number> getNumber(String content) {
                try {
                    return Optional.of(BigDecimal.valueOf(Long.decode(content)));
                } catch (NumberFormatException e) {
                    try {
                        return Optional.of(new BigDecimal(content));
                    } catch (Exception exception) {
                        return Optional.empty();
                    }
                }
            }
        };
    }

    static TokenFactory createBeanPropertyTokenFactory() {
        return new TokenFactoryBase(
                StartWith.excluded(ContextMatches.CHARACTER('.')),
                //TODO to be refactor
                Content.leftTrim(Content.allChars()),
                EndWith.NO_MORE_SOURCE_CODE.or(EndWith.before(ContextMatches.TOKEN_DELIMITER))) {
            @Override
            protected Token createToken(String content) {
                if (content.isEmpty())
                    throw new IllegalTokenContentException("property chain not finished");
                return Token.propertyToken(content.split("\\."));
            }
        };
    }

    static TokenFactory createOperatorTokenFactory() {
        return new TokenFactoryBase(
                StartWith.included(ContextMatches.OPERATOR.except(ContextMatches.CHARACTER('/').when(ContextPredicate.AFTER_TOKEN_MATCHES))),
                Content.allChars(),
                EndWith.NO_MORE_SOURCE_CODE
                        .or(EndWith.before(ContextMatches.not(ContextMatches.OPERATOR)))
                        .or(EndWith.before(ContextMatches.CHARACTER('/').when(ContextPredicate.AFTER_OPERATOR_MATCHES)))) {

            @Override
            protected Token createToken(String content) {
                return Token.operatorToken(content);
            }
        };
    }

    static TokenFactory createSingleQuotedStringTokenFactory() {
        return new TokenFactoryBase(
                StartWith.excluded(ContextMatches.CHARACTER('\'')),
                Content.allChars()
                        .escape("\\'", '\'')
                        .escape("\\\\", '\\'),
                EndWith.excluded(ContextMatches.CHARACTER('\'')).orThrow("string should end with `'`")) {

            @Override
            protected Token createToken(String content) {
                return Token.constValueToken(content);
            }
        };
    }

    static TokenFactory createDoubleQuotedStringTokenFactory() {
        return new TokenFactoryBase(
                StartWith.excluded(ContextMatches.CHARACTER('"')),
                Content.allChars()
                        .escape("\\\"", '"')
                        .escape("\\t", '\t')
                        .escape("\\n", '\n')
                        .escape("\\\\", '\\'),
                EndWith.excluded(ContextMatches.CHARACTER('"')).orThrow("string should end with `\"`")) {

            @Override
            protected Token createToken(String content) {
                return Token.constValueToken(content);
            }
        };
    }

    static TokenFactory createRegexTokenFactory() {
        return new TokenFactoryBase(
                StartWith.excluded(ContextMatches.CHARACTER('/').when(ContextPredicate.AFTER_TOKEN_MATCHES)),
                Content.allChars()
                        .escape("\\\\", '\\')
                        .escape("\\/", '/'),
                EndWith.excluded(ContextMatches.CHARACTER('/')).orThrow("regex should end with `/`")) {

            @Override
            protected Token createToken(String content) {
                return Token.regexToken(content);
            }
        };
    }

    Token fetchToken(SourceCode sourceCode, Token previous);
}
