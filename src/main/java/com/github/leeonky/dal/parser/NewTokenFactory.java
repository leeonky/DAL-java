package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.IllegalTokenContentException;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;

import java.math.BigDecimal;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

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

    private final TokenStartEnd start;

    private NewTokenFactory(TokenStartEnd start) {
        this.start = start;
    }

    public static NewTokenFactory startWith(TokenStartEnd startEnd) {
        return new NewTokenFactory(startEnd);
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

    public Content take(SourceCodeGetter sourceCodeGetter) {
        return new Content(sourceCodeGetter);
    }

    public class Content {

        private final SourceCodeGetter sourceCodeGetter;

        public Content(SourceCodeGetter sourceCodeGetter) {
            this.sourceCodeGetter = sourceCodeGetter;
        }

        public End endWith(TokenStartEnd end) {
            return new End(end);
        }

        public class End {
            private final TokenStartEnd end;

            public End(TokenStartEnd end) {
                this.end = end;
            }

            public TokenFactory createAs(Function<String, Token> creator) {
                return (sourceCode, previous) -> {
                    ParseContext context = new ParseContext(sourceCode, previous);
                    if (start.matches(context)) {
                        sourceCodeGetter.preprocess(context.sourceCode);
                        while (!end.matches(context))
                            context.content.add(sourceCodeGetter.getChar(context.sourceCode));
                        try {
                            return creator.apply(context.content.stream().map(Objects::toString).collect(Collectors.joining()));
                        } catch (IllegalTokenContentException e) {
                            throw new SyntaxException(sourceCode.getPosition(), e.getMessage());
                        }
                    }
                    return null;
                };
            }
        }
    }
}
