package com.github.leeonky.dal.token;

import com.github.leeonky.dal.parser.ParsingContext;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import static com.github.leeonky.dal.DALCompiler.*;
import static com.github.leeonky.dal.parser.NewTokenFactory.equalToCharacter;
import static com.github.leeonky.dal.parser.NewTokenFactory.startWith;
import static com.github.leeonky.dal.parser.ParsingContext.*;
import static com.github.leeonky.dal.parser.SourceCodeMatcher.not;
import static com.github.leeonky.dal.parser.TokenContentInString.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenContentInString.leftTrim;
import static com.github.leeonky.dal.parser.TokenContentInToken.byFactory;
import static com.github.leeonky.dal.parser.TokenStartEnd.before;
import static com.github.leeonky.dal.token.Token.*;

public interface TokenFactory {
    Function<String, Token> CONST_NUMBER_TOKEN = content ->
            getNumber(content).map(Token::constValueToken).orElse(null);
    Function<String, Token> OPERATOR_TOKEN = Token::operatorToken;
    Function<String, Token> PROPERTY_TOKEN = content -> {
        if (content.isEmpty())
            throw new IllegalTokenContentException("property chain not finished");
        return Token.propertyToken(content);
    };
    Function<String, Token> CONST_STRING_TOKEN = Token::constValueToken;
    Function<String, Token> REGEX_TOKEN = Token::regexToken;
    Function<String, Token> BEGIN_BRACKET_TOKEN = s -> Token.beginBracketToken();
    Function<String, Token> END_BRACKET_TOKEN = s -> Token.endBracketToken();
    Function<List<Token>, Token> BRACKET_PROPERTY_TOKEN = token -> {
        if (token.size() != 1)
            throw new IllegalTokenContentException("should given one property or array index in `[]`");
        return Token.propertyToken(token.get(0).getPropertyOrIndex());
    };
    Function<String, Token> WORD_TOKEN = content -> {
        if (NULL.equals(content))
            return constValueToken(null);
        if (TRUE.equals(content))
            return constValueToken(true);
        if (FALSE.equals(content))
            return constValueToken(false);
        if (AND.equals(content))
            return operatorToken("&&");
        if (OR.equals(content))
            return operatorToken("||");
        if (MATCHES.equals(content))
            return operatorToken(MATCHES);
        if (Scanner.KEYWORD_SETS.contains(content))
            return keyWordToken(content);
        return wordToken(content);
    };

    static TokenFactory createNumberTokenFactory() {
        return startWith(included(DIGITAL))
                .endWith(END_OF_CODE.or(before(DELIMITER)))
                .createAs(CONST_NUMBER_TOKEN);
    }

    static TokenFactory createBeanPropertyTokenFactory() {
        return startWith(excluded(CHARACTER('.')))
                .take(leftTrim(ALL_CHARACTERS))
                .endWith(END_OF_CODE.or(before(DELIMITER)).or(before(CHARACTER('.'))))
                .createAs(PROPERTY_TOKEN);
    }

    static TokenFactory createOperatorTokenFactory() {
        return startWith(included(OPERATOR.except(CHARACTER('/').when(AFTER_TOKEN_MATCHES))))
                .endWith(END_OF_CODE.or(before(not(OPERATOR))).or(before(CHARACTER('/').when(AFTER_OPERATOR_MATCHES))))
                .createAs(OPERATOR_TOKEN);
    }

    static TokenFactory createSingleQuotedStringTokenFactory() {
        return startWith(excluded(CHARACTER('\'')))
                .take(ALL_CHARACTERS
                        .escape("\\'", '\'')
                        .escape("\\\\", '\\'))
                .endWith(excluded(CHARACTER('\'')).orThrow("string should end with `'`"))
                .createAs(CONST_STRING_TOKEN);
    }

    static TokenFactory createDoubleQuotedStringTokenFactory() {
        return startWith(excluded(CHARACTER('"')))
                .take(ALL_CHARACTERS
                        .escape("\\\"", '"')
                        .escape("\\t", '\t')
                        .escape("\\n", '\n')
                        .escape("\\\\", '\\'))
                .endWith(excluded(CHARACTER('"')).orThrow("string should end with `\"`"))
                .createAs(CONST_STRING_TOKEN);
    }

    static TokenFactory createRegexTokenFactory() {
        return startWith(excluded(CHARACTER('/').when(AFTER_TOKEN_MATCHES)))
                .take(ALL_CHARACTERS
                        .escape("\\\\", '\\')
                        .escape("\\/", '/'))
                .endWith(excluded(CHARACTER('/')).orThrow("string should end with `/`"))
                .createAs(REGEX_TOKEN);
    }

    static TokenFactory createBeginBracketTokenFactory() {
        return equalToCharacter('(').createAs(BEGIN_BRACKET_TOKEN);
    }

    static TokenFactory createEndBracketTokenFactory() {
        return equalToCharacter(')').createAs(END_BRACKET_TOKEN);
    }

    static TokenFactory createBracketPropertyTokenFactory() {
        return startWith(excluded(CHARACTER('[').except(AFTER_TOKEN_MATCHES)))
                .take(byFactory(createNumberTokenFactory())
                        .or(createSingleQuotedStringTokenFactory())
                        .or(createDoubleQuotedStringTokenFactory()))
                .endWith(excluded(CHARACTER(']')).orThrow("should end with `]`"))
                .createAs(BRACKET_PROPERTY_TOKEN);
    }

    static TokenFactory createWordTokenFactory() {
        return startWith(included(ANY_CHARACTERS))
                .take(ALL_CHARACTERS)
                .endWith(END_OF_CODE.or(before(DELIMITER)))
                .createAs(WORD_TOKEN);
    }

    static Optional<Number> getNumber(String content) {
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

    Token fetchToken(ParsingContext context);
}
