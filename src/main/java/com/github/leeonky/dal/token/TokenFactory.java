package com.github.leeonky.dal.token;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.parser.TokenParser;

import java.math.BigDecimal;
import java.util.Optional;
import java.util.function.Function;

import static com.github.leeonky.dal.Constants.KeyWords.*;
import static com.github.leeonky.dal.Constants.Operators;
import static com.github.leeonky.dal.parser.NewTokenFactory.equalToCharacter;
import static com.github.leeonky.dal.parser.NewTokenFactory.startsWith;
import static com.github.leeonky.dal.parser.SourceCodeMatcher.not;
import static com.github.leeonky.dal.parser.TokenContentInString.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenContentInString.leftTrim;
import static com.github.leeonky.dal.parser.TokenContentInToken.byFactory;
import static com.github.leeonky.dal.parser.TokenParser.ANY_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenParser.BEGIN_OF_CODE;
import static com.github.leeonky.dal.parser.TokenParser.CHARACTER;
import static com.github.leeonky.dal.parser.TokenParser.DELIMITER;
import static com.github.leeonky.dal.parser.TokenParser.DIGITAL;
import static com.github.leeonky.dal.parser.TokenParser.END_OF_CODE;
import static com.github.leeonky.dal.parser.TokenParser.OPERATOR;
import static com.github.leeonky.dal.parser.TokenParser.OPERATOR_CHAR;
import static com.github.leeonky.dal.parser.TokenParser.after;
import static com.github.leeonky.dal.parser.TokenParser.excluded;
import static com.github.leeonky.dal.parser.TokenParser.included;
import static com.github.leeonky.dal.parser.TokenParser.startsWith;
import static com.github.leeonky.dal.parser.TokenStartEnd.before;
import static com.github.leeonky.dal.token.Token.*;

public interface TokenFactory {
    Function<String, Token> CONST_NUMBER_TOKEN = content ->
            getNumber(content).map(Token::constValueToken).orElse(null);
    Function<String, Token> OPERATOR_TOKEN = Token::operatorToken;
    Function<String, Token> PROPERTY_TOKEN = content -> {
        if (content.isEmpty())
            throw new IllegalTokenContentException("property not finished");
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
    Function<TokenStream, Token> BRACKET_PROPERTY_TOKEN = tokenStream ->
            Token.propertyToken(tokenStream.popOnlyOneTokenForPropertyOnIndex());

    Function<String, Token> IDENTIFIER_TOKEN = content -> {
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
        return identifierToken(content);
    };
    Function<TokenStream, Token> TOKEN_TREE = Token::treeToken;

    static TokenFactory createNumberTokenFactory() {
        return startsWith(included(DIGITAL))
                .endWith(END_OF_CODE.or(before(DELIMITER)))
                .createAs(CONST_NUMBER_TOKEN);
    }

    static TokenFactory createBeanPropertyTokenFactory() {
        return startsWith(excluded(CHARACTER('.')))
                .take(leftTrim(ALL_CHARACTERS))
                .endWith(END_OF_CODE.or(before(DELIMITER)).or(before(CHARACTER('.'))))
                .createAs(PROPERTY_TOKEN);
    }

    static TokenFactory createOperatorTokenFactory() {
        return startsWith(included(OPERATOR_CHAR
                .or(CHARACTER('.').when(startsWith("...")))
                .except(CHARACTER('/').when(after(Token::isJudgement))))
        )
                .endWith(END_OF_CODE.or(before(not(OPERATOR)))
                        .or(before(CHARACTER('/').when(after(Operators.MATCH).or(after(Operators.EQ))))))
                .createAs(OPERATOR_TOKEN);
    }

    static TokenFactory createSingleQuotedStringTokenFactory() {
        return startsWith(excluded(CHARACTER('\'')))
                .take(ALL_CHARACTERS
                        .escape("\\'", '\'')
                        .escape("\\\\", '\\'))
                .endWith(excluded(CHARACTER('\'')).orThrow("string should end with `'`"))
                .createAs(CONST_STRING_TOKEN);
    }

    static TokenFactory createDoubleQuotedStringTokenFactory() {
        return startsWith(excluded(CHARACTER('"')))
                .take(ALL_CHARACTERS
                        .escape("\\\"", '"')
                        .escape("\\t", '\t')
                        .escape("\\n", '\n')
                        .escape("\\\\", '\\'))
                .endWith(excluded(CHARACTER('"')).orThrow("string should end with `\"`"))
                .createAs(CONST_STRING_TOKEN);
    }

    static TokenFactory createRegexTokenFactory() {
        return startsWith(excluded(CHARACTER('/').when(after(Token::isJudgement))))
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
        return startsWith(excluded(CHARACTER('[').except(after(Token::isJudgement))))
                .take(byFactory(createNumberTokenFactory())
                        .or(createSingleQuotedStringTokenFactory())
                        .or(createDoubleQuotedStringTokenFactory()))
                .endWith(excluded(CHARACTER(']')).orThrow("should end with `]`"))
                .createAs(BRACKET_PROPERTY_TOKEN);
    }

    static TokenFactory createIdentifierTokenFactory() {
        return startsWith(included(ANY_CHARACTERS))
                .take(ALL_CHARACTERS)
                .endWith(END_OF_CODE.or(before(DELIMITER)))
                .createAs(IDENTIFIER_TOKEN);
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
        return startsWith(BEGIN_OF_CODE)
                .take(byFactory(createOperatorTokenFactory())
                        .or(createNumberTokenFactory())
                        .or(createSingleQuotedStringTokenFactory())
                        .or(createDoubleQuotedStringTokenFactory())
                        .or(createRegexTokenFactory())
                        .or(createBeanPropertyTokenFactory())
                        .or(createOpeningParenthesisTokenFactory())
                        .or(createClosingParenthesisTokenFactory())
                        .or(createOpeningBraceTokenFactory())
                        .or(createClosingBraceTokenFactory())
                        .or(createOpeningBracketTokenFactory())
                        .or(createClosingBracketTokenFactory())
                        .or(createIdentifierTokenFactory()))
                .endWith(END_OF_CODE)
                .createAs(TOKEN_TREE);
    }

    static TokenFactory createPropertyChainFactory() {
        return startsWith(BEGIN_OF_CODE)
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
