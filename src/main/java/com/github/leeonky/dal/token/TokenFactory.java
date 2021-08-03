package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.token.Token.constValueToken;
import static com.github.leeonky.dal.token.Token.propertyToken;

public interface TokenFactory {
    static TokenFactory createNumberTokenFactory() {
        return new TokenFactoryBase(
                StartWith.DIGITAL,
                LeftTrim.NOTHING,
                EndWith.TOKEN_DELIMITER) {
            @Override
            protected Token createToken(String content) {
                try {
                    return constValueToken(BigDecimal.valueOf(Long.decode(content)));
                } catch (NumberFormatException ignore) {
                    return constValueToken(new BigDecimal(content));
                }
            }
        };
    }

    static TokenFactory createPropertyTokenFactory() {
        return new TokenFactoryBase(
                StartWith.CHARACTER('.'),
                LeftTrim.TRIM_FIRST_CHAR_AND_WHITESPACE,
                EndWith.TOKEN_DELIMITER) {
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
                StartWith.OPERATOR_BUT_NOT_REGEX_AFTER_MATCHES,
                LeftTrim.NOTHING,
                EndWith.NOT_OPERATOR_OR_REGEX_AFTER_MATCHES) {
            @Override
            protected Token createToken(String content) {
                return Token.operatorToken(content);
            }
        };
    }

    Token fetchToken(SourceCode sourceCode, Token previous);
}

interface StartWith {
    StartWith DIGITAL = (sourceCode, previous) -> Character.isDigit(sourceCode.getChar());
    StartWith OPERATOR = (sourceCode, previous) -> Scanner.OPERATOR_CHAR.contains(sourceCode.getChar());
    StartWith OPERATOR_BUT_NOT_REGEX_AFTER_MATCHES = (sourceCode, previous) -> OPERATOR.matches(sourceCode, previous)
            && !(sourceCode.getChar() == '/' && previous != null && previous.isOperatorMatches());

    static StartWith CHARACTER(char c) {
        return (sourceCode, previous) -> sourceCode.getChar() == c;
    }

    boolean matches(SourceCode sourceCode, Token previous);
}

interface LeftTrim {
    LeftTrim NOTHING = sourceCode -> sourceCode;
    LeftTrim TRIM_FIRST_CHAR = sourceCode -> {
        sourceCode.takeChar();
        return sourceCode;
    };
    LeftTrim TRIM_FIRST_CHAR_AND_WHITESPACE = sourceCode -> TRIM_FIRST_CHAR.process(sourceCode).trimLeft();

    SourceCode process(SourceCode sourceCode);
}

interface EndWith {
    EndWith TOKEN_DELIMITER = (content, o) -> Scanner.TOKEN_DELIMITER.contains(o);
    EndWith NOT_OPERATOR_OR_REGEX_AFTER_MATCHES = (content, c) -> !Scanner.OPERATOR_CHAR.contains(c)
            || content.size() == 1 && content.get(0).equals('~');

    boolean matches(List<Character> content, char c);
}

abstract class TokenFactoryBase implements TokenFactory {
    private final StartWith startWith;
    private final LeftTrim leftTrim;
    private final EndWith endWith;

    protected TokenFactoryBase(StartWith startWith, LeftTrim leftTrim, EndWith endWith) {
        this.startWith = startWith;
        this.leftTrim = leftTrim;
        this.endWith = endWith;
    }

    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        if (sourceCode.notEnd() && startWith.matches(sourceCode, previous))
            return createToken(sourceCode, parseContent(sourceCode));
        return null;
    }

    private String parseContent(SourceCode sourceCode) {
        leftTrim.process(sourceCode);
        List<Character> content = new ArrayList<>();
        while (sourceCode.notEnd() && !endWith.matches(content, sourceCode.getChar()))
            content.add(sourceCode.takeChar());
        return content.stream().map(Objects::toString).collect(Collectors.joining());
    }

    private Token createToken(SourceCode sourceCode, String collect) {
        try {
            return createToken(collect);
        } catch (IllegalTokenContentException e) {
            throw new SyntaxException(sourceCode.getPosition(), e.getMessage());
        }
    }

    protected abstract Token createToken(String content);
}
