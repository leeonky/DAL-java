package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.IllegalTokenContentException;
import com.github.leeonky.dal.token.Scanner;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import static com.github.leeonky.dal.DALCompiler.*;
import static com.github.leeonky.dal.parser.ParsingContext.ANY_CHARACTERS;
import static com.github.leeonky.dal.parser.ParsingContext.CHARACTER;
import static com.github.leeonky.dal.parser.TokenContentInString.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenStartEnd.before;
import static com.github.leeonky.dal.token.Token.*;

public class NewTokenFactory {
    public static final Function<String, Token> CONST_NUMBER_TOKEN = content ->
            getNumber(content).map(Token::constValueToken).orElse(null);
    public static final Function<String, Token> OPERATOR_TOKEN = Token::operatorToken;
    public static final Function<String, Token> PROPERTY_TOKEN = content -> {
        if (content.isEmpty())
            throw new IllegalTokenContentException("property chain not finished");
        return Token.propertyToken(content);
    };
    public static final Function<String, Token> CONST_STRING_TOKEN = Token::constValueToken;
    public static final Function<String, Token> REGEX_TOKEN = Token::regexToken;
    public static final Function<String, Token> BEGIN_BRACKET_TOKEN = s -> Token.beginBracketToken();
    public static final Function<String, Token> END_BRACKET_TOKEN = s -> Token.endBracketToken();
    public static final Function<List<Token>, Token> BRACKET_PROPERTY_TOKEN = token -> {
        if (token.size() != 1)
            throw new IllegalTokenContentException("should given one property or array index in `[]`");
        return Token.propertyToken(token.get(0).getPropertyOrIndex());
    };
    public static final Function<String, Token> WORD_TOKEN = content -> {
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

    private final TokenStartEnd start;

    private NewTokenFactory(TokenStartEnd start) {
        this.start = start;
    }

    public static NewTokenFactory startWith(TokenStartEnd startEnd) {
        return new NewTokenFactory(startEnd);
    }

    public static StringContent.EndWith equalToCharacter(char c) {
        return startWith(ParsingContext.included(CHARACTER(c))).take(ALL_CHARACTERS).endWith(ParsingContext.END_OF_CODE.or(before(ANY_CHARACTERS)));
    }

    private static Optional<Number> getNumber(String content) {
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

    public StringContent take(TokenContentInString content) {
        return new StringContent(content);
    }

    public StringContent.EndWith endWith(TokenStartEnd end) {
        return take(ALL_CHARACTERS).endWith(end);
    }

    public TokenContent take(TokenContentInToken content) {
        return new TokenContent(content);
    }

    public class StringContent {

        private final TokenContentInString content;

        public StringContent(TokenContentInString content) {
            this.content = content;
        }

        public EndWith endWith(TokenStartEnd end) {
            return new EndWith(end);
        }

        public class EndWith {
            private final TokenStartEnd end;

            public EndWith(TokenStartEnd end) {
                this.end = end;
            }

            public TokenFactory createAs(Function<String, Token> creator) {
                return context -> context.parseToken(start, content, end, creator);
            }
        }
    }

    public class TokenContent {
        private final TokenContentInToken content;

        public TokenContent(TokenContentInToken content) {
            this.content = content;
        }

        public EndWith endWith(TokenStartEnd end) {
            return new EndWith(end);
        }

        public class EndWith {
            private final TokenStartEnd end;

            public EndWith(TokenStartEnd end) {
                this.end = end;
            }

            public TokenFactory createAs(Function<List<Token>, Token> creator) {
                return context -> context.parseToken(start, content, end, creator);
            }
        }
    }
}
