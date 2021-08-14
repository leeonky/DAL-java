package com.github.leeonky.dal.token;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.parser.TokenParser;

import java.math.BigDecimal;
import java.util.Optional;
import java.util.function.Function;

import static com.github.leeonky.dal.Constants.KeyWords.*;
import static com.github.leeonky.dal.Constants.Operators;
import static com.github.leeonky.dal.parser.NewTokenFactory.equalToCharacter;
import static com.github.leeonky.dal.parser.NewTokenFactory.startWith;
import static com.github.leeonky.dal.parser.SourceCodeMatcher.not;
import static com.github.leeonky.dal.parser.TokenContentInString.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenContentInString.leftTrim;
import static com.github.leeonky.dal.parser.TokenContentInToken.byFactory;
import static com.github.leeonky.dal.parser.TokenParser.*;
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
    Function<String, Token> OPENING_PARENTHESIS_TOKEN = s -> Token.openingParenthesisToken();
    Function<String, Token> CLOSING_PARENTHESIS_TOKEN = s -> Token.closingParenthesisToken();
    Function<String, Token> OPENING_BRACKET_TOKEN = s -> Token.openingBracketToken();
    Function<String, Token> CLOSING_BRACKET_TOKEN = s -> Token.closingBracketToken();
    Function<String, Token> OPENING_BRACE_TOKEN = s -> Token.openingBraceToken();
    Function<String, Token> CLOSING_BRACE_TOKEN = s -> Token.closingBraceToken();
    Function<TokenStream, Token> BRACKET_PROPERTY_TOKEN = tokenStream -> {
        if (tokenStream.size() != 1)
            throw new IllegalTokenContentException("should given one property or array index in `[]`");
        return Token.propertyToken(tokenStream.pop().getPropertyOrIndex());
    };
    Function<String, Token> WORD_TOKEN = content -> {
        if (NULL.equals(content))
            return constValueToken(null);
        if (TRUE.equals(content))
            return constValueToken(true);
        if (FALSE.equals(content))
            return constValueToken(false);
        if (AND.equals(content))
            return operatorToken(AND);
        if (OR.equals(content))
            return operatorToken(OR);
        if (Constants.KEYWORD_SETS.contains(content))
            return keyWordToken(content);
        return wordToken(content);
    };
    Function<TokenStream, Token> TOKEN_TREE = Token::treeToken;

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
        return startWith(included(OPERATOR.except(CHARACTER('/').when(after(Token::judgement)))))
                .endWith(END_OF_CODE.or(before(not(OPERATOR))).or(before(CHARACTER('/').when(after(Operators.MATCH).or(after(Operators.EQ))))))
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
        return startWith(excluded(CHARACTER('/').when(after(Token::judgement))))
                .take(ALL_CHARACTERS
                        .escape("\\\\", '\\')
                        .escape("\\/", '/'))
                .endWith(excluded(CHARACTER('/')).orThrow("string should end with `/`"))
                .createAs(REGEX_TOKEN);
    }

    static TokenFactory createOpeningParenthesisTokenFactory() {
        return equalToCharacter('(').createAs(OPENING_PARENTHESIS_TOKEN);
    }

    static TokenFactory createClosingParenthesisTokenFactory() {
        return equalToCharacter(')').createAs(CLOSING_PARENTHESIS_TOKEN);
    }

    static TokenFactory createBracketPropertyTokenFactory() {
        return startWith(excluded(CHARACTER('[').except(after(Token::judgement))))
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

    static TokenFactory createDALTokenFactory() {
        return startWith(BEGIN_OF_CODE)
                .take(byFactory(createBeanPropertyTokenFactory())
                        .or(createNumberTokenFactory())
                        .or(createSingleQuotedStringTokenFactory())
                        .or(createDoubleQuotedStringTokenFactory())
                        .or(createRegexTokenFactory())
                        .or(createOperatorTokenFactory())
                        .or(createOpeningParenthesisTokenFactory())
                        .or(createClosingParenthesisTokenFactory())
                        .or(createOpeningBraceTokenFactory())
                        .or(createClosingBraceTokenFactory())
                        .or(createOpeningBracketTokenFactory())
                        .or(createClosingBracketTokenFactory())
                        .or(createWordTokenFactory()))
                .endWith(END_OF_CODE)
                .createAs(TOKEN_TREE);
    }

    static TokenFactory createPropertyChainFactory() {
        return startWith(BEGIN_OF_CODE)
                .take(byFactory(createBeanPropertyTokenFactory())
                        .or(createBracketPropertyTokenFactory()))
                .endWith(END_OF_CODE).createAs(TOKEN_TREE);
    }

    static TokenFactory createOpeningBracketTokenFactory() {
        return equalToCharacter('[').createAs(OPENING_BRACKET_TOKEN);
    }

    static TokenFactory createClosingBracketTokenFactory() {
        return equalToCharacter(']').createAs(CLOSING_BRACKET_TOKEN);
    }

    static TokenFactory createOpeningBraceTokenFactory() {
        return equalToCharacter('{').createAs(OPENING_BRACE_TOKEN);
    }

    static TokenFactory createClosingBraceTokenFactory() {
        return equalToCharacter('}').createAs(CLOSING_BRACE_TOKEN);
    }

    Token fetchToken(TokenParser parser);
}
