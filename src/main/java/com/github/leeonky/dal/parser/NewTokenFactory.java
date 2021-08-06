package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.IllegalTokenContentException;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;

import java.math.BigDecimal;
import java.util.Optional;
import java.util.function.Function;

import static com.github.leeonky.dal.parser.ParsingContext.ANY_CHARACTERS;
import static com.github.leeonky.dal.parser.ParsingContext.CHARACTER;
import static com.github.leeonky.dal.parser.TokenContent.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenStartEnd.before;

public class NewTokenFactory {
    public static final Function<String, Token> CONST_NUMBER_TOKEN = content ->
            getNumber(content).map(Token::constValueToken).orElse(null);
    public static final Function<String, Token> OPERATOR_TOKEN = Token::operatorToken;
    public static final Function<String, Token> PROPERTY_TOKEN = content -> {
        if (content.isEmpty())
            throw new IllegalTokenContentException("property chain not finished");
        return Token.propertyToken(content.split("\\."));
    };
    public static final Function<String, Token> CONST_STRING_TOKEN = Token::constValueToken;
    public static final Function<String, Token> REGEX_TOKEN = Token::regexToken;
    public static final Function<String, Token> BEGIN_BRACKET_TOKEN = s -> Token.beginBracketToken();
    public static final Function<String, Token> END_BRACKET_TOKEN = s -> Token.endBracketToken();

    private final TokenStartEnd start;

    private NewTokenFactory(TokenStartEnd start) {
        this.start = start;
    }

    public static NewTokenFactory startWith(TokenStartEnd startEnd) {
        return new NewTokenFactory(startEnd);
    }

    public static Content.EndWith equalToCharacter(char c) {
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

    public Content take(TokenContent tokenContent) {
        return new Content(tokenContent);
    }

    public Content.EndWith endWith(TokenStartEnd end) {
        return take(ALL_CHARACTERS).endWith(end);
    }

    public class Content {

        private final TokenContent tokenContent;

        public Content(TokenContent tokenContent) {
            this.tokenContent = tokenContent;
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
                return context -> context.parseToken(start, tokenContent, end, creator);
            }
        }
    }
}
