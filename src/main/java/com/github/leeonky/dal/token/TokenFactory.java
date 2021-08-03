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
                EndWith.NO_MORE_SOURCE_CODE.or(EndWith.TOKEN_DELIMITER)) {
            @Override
            protected Token createToken(String content) {
                try {
                    return constValueToken(BigDecimal.valueOf(Long.decode(content)));
                } catch (NumberFormatException ignore) {
                    return constValueToken(new BigDecimal(content));
                }
            }

            @Override
            public Token fetchToken(SourceCode sourceCode, Token previous) {
                try {
                    return super.fetchToken(sourceCode, previous);
                } catch (NumberFormatException ignore) {
                    return null;
                }
            }
        };
    }

    static TokenFactory createBeanPropertyTokenFactory() {
        return new TokenFactoryBase(
                StartWith.CHARACTER('.'),
                LeftTrim.TRIM_FIRST_CHAR_AND_WHITESPACE,
                EndWith.NO_MORE_SOURCE_CODE.or(EndWith.TOKEN_DELIMITER)) {
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
                StartWith.OPERATOR.and(StartWith.not(StartWith.REGEX_AFTER_MATCHES)),
                LeftTrim.NOTHING,
                EndWith.NO_MORE_SOURCE_CODE.or(EndWith.NOT_OPERATOR).or(EndWith.REGEX_AFTER_MATCHES)) {
            @Override
            protected Token createToken(String content) {
                return Token.operatorToken(content);
            }
        };
    }

    static TokenFactory createRawTextPropertyTokenFactory() {
        return new TokenFactoryBase(
                StartWith.ANY_CHAR,
                LeftTrim.NOTHING,
                EndWith.CHARACTER(']').orThrow("property chain not finished")) {

            @Override
            protected Token createToken(String content) {
                try {
                    return Token.constIndexToken(Integer.decode(content));
                } catch (NumberFormatException ignore) {
                    return Token.propertyToken(content);
                }
            }
        };
    }

    Token fetchToken(SourceCode sourceCode, Token previous);
}

interface StartWith {
    StartWith DIGITAL = (sourceCode, previous) -> Character.isDigit(sourceCode.currentChar());
    StartWith OPERATOR = (sourceCode, previous) -> Scanner.OPERATOR_CHAR.contains(sourceCode.currentChar());
    StartWith REGEX_AFTER_MATCHES = (sourceCode, previous) ->
            sourceCode.currentChar() == '/' && previous != null && previous.isOperatorMatches();
    StartWith ANY_CHAR = (sourceCode, previous) -> true;

    static StartWith CHARACTER(char c) {
        return (sourceCode, previous) -> sourceCode.currentChar() == c;
    }

    static StartWith not(StartWith startWith) {
        return (sourceCode, previous) -> !startWith.matches(sourceCode, previous);
    }

    boolean matches(SourceCode sourceCode, Token previous);

    default StartWith and(StartWith another) {
        return (sourceCode, previous) -> matches(sourceCode, previous) && another.matches(sourceCode, previous);
    }
}

interface LeftTrim {
    LeftTrim NOTHING = sourceCode -> sourceCode;
    LeftTrim TRIM_FIRST_CHAR = sourceCode -> {
        sourceCode.takeCurrentChar();
        return sourceCode;
    };
    LeftTrim TRIM_FIRST_CHAR_AND_WHITESPACE = sourceCode -> TRIM_FIRST_CHAR.process(sourceCode).trimLeft();

    SourceCode process(SourceCode sourceCode);
}

interface EndWith {
    EndWith TOKEN_DELIMITER = (sourceCode, content) -> Scanner.TOKEN_DELIMITER.contains(sourceCode.currentChar());
    EndWith NO_MORE_SOURCE_CODE = (sourceCode, content) -> !sourceCode.notEnd();
    EndWith REGEX_AFTER_MATCHES = (sourceCode, content) -> content.size() == 1 && content.get(0).equals('~');
    EndWith NOT_OPERATOR = (sourceCode, content) -> !Scanner.OPERATOR_CHAR.contains(sourceCode.currentChar());

    static EndWith CHARACTER(char endChar) {
        return (sourceCode, content) -> Objects.equals(sourceCode.currentChar(), endChar);
    }

    boolean matches(SourceCode sourceCode, List<Character> content);

    default EndWith or(EndWith another) {
        return (sourceCode, content) -> matches(sourceCode, content) || another.matches(sourceCode, content);
    }

    default EndWith orThrow(String errorMessage) {
        return (sourceCode, content) -> {
            try {
                return matches(sourceCode, content);
            } catch (NoMoreSourceCodeException ignore) {
                throw new SyntaxException(sourceCode.getPosition(), errorMessage);
            }
        };
    }
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
            try {
                return createToken(parseContent(leftTrim.process(sourceCode)));
            } catch (IllegalTokenContentException e) {
                throw new SyntaxException(sourceCode.getPosition(), e.getMessage());
            }
        return null;
    }

    private String parseContent(SourceCode sourceCode) {
        List<Character> content = new ArrayList<>();
        while (!endWith.matches(sourceCode, content))
            content.add(sourceCode.takeCurrentChar());
        return content.stream().map(Objects::toString).collect(Collectors.joining());
    }

    protected abstract Token createToken(String content);
}
