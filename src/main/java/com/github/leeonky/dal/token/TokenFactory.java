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
    Token fetchToken(SourceCode sourceCode, Token previous);
}

interface StartWith {
    StartWith DIGITAL = sourceCode -> Character.isDigit(sourceCode.getChar());

    static StartWith CHARACTER(char c) {
        return sourceCode -> sourceCode.getChar() == c;
    }

    boolean matches(SourceCode sourceCode);
}

interface FirstChar {
    FirstChar TRIM = SourceCode::takeChar;
    FirstChar NOT_TRIM = sourceCode -> {
    };

    void process(SourceCode sourceCode);
}

interface StartWhiteSpace {
    StartWhiteSpace TRIM = () -> true;
    StartWhiteSpace NO_TRIM = () -> false;

    boolean isTrim();
}

interface EndWith {
    EndWith TOKEN_DELIMITER = Scanner.TOKEN_DELIMITER::contains;

    boolean matches(char c);
}

abstract class TokenFactoryBase implements TokenFactory {
    private final StartWith startWith;
    private final FirstChar firstChar;
    private final StartWhiteSpace startWhiteSpace;
    private final EndWith endWith;

    protected TokenFactoryBase(StartWith startWith, FirstChar firstChar, StartWhiteSpace startWhiteSpace, EndWith endWith) {
        this.startWith = startWith;
        this.firstChar = firstChar;
        this.startWhiteSpace = startWhiteSpace;
        this.endWith = endWith;
    }

    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        if (sourceCode.notEnd() && startWith.matches(sourceCode)) {
            firstChar.process(sourceCode);
            while (sourceCode.isWhitespaceChar() && startWhiteSpace.isTrim())
                sourceCode.takeChar();
            List<Character> content = new ArrayList<>();
            while (sourceCode.notEnd() && !endWith.matches(sourceCode.getChar()))
                content.add(sourceCode.takeChar());
            try {
                return createToken(content.stream().map(Objects::toString).collect(Collectors.joining()));
            } catch (IllegalTokenContentException e) {
                throw new SyntaxException(sourceCode.getPosition(), e.getMessage());
            }
        }
        return null;
    }

    protected abstract Token createToken(String content);
}

class NumberTokenFactory extends TokenFactoryBase {

    NumberTokenFactory() {
        super(StartWith.DIGITAL, FirstChar.NOT_TRIM, StartWhiteSpace.NO_TRIM, EndWith.TOKEN_DELIMITER);
    }

    @Override
    protected Token createToken(String content) {
        try {
            return constValueToken(BigDecimal.valueOf(Long.decode(content)));
        } catch (NumberFormatException ignore) {
            return constValueToken(new BigDecimal(content));
        }
    }
}

class PropertyTokenFactory extends TokenFactoryBase {

    PropertyTokenFactory() {
        super(StartWith.CHARACTER('.'), FirstChar.TRIM, StartWhiteSpace.TRIM, EndWith.TOKEN_DELIMITER);
    }

    @Override
    protected Token createToken(String content) {
        if (content.isEmpty())
            throw new IllegalTokenContentException("property chain not finished");
        return propertyToken(content.split("\\."));
    }
}
