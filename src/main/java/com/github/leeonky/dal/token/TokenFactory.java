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

abstract class TokenFactoryBase implements TokenFactory {
    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        if (sourceCode.notEnd() && isMatched(sourceCode)) {
            while (sourceCode.notEnd() && isLeftTrim(sourceCode.getChar()))
                sourceCode.takeChar();
            List<Character> content = new ArrayList<>();
            while (sourceCode.notEnd() && !isEnded(sourceCode.getChar()))
                content.add(sourceCode.takeChar());
            try {
                return createToken(content.stream().map(Objects::toString).collect(Collectors.joining()));
            } catch (IllegalTokenContentException e) {
                throw new SyntaxException(sourceCode.getPosition(), e.getMessage());
            }
        }
        return null;
    }

    protected abstract boolean isLeftTrim(char c);

    protected abstract boolean isEnded(char c);

    protected abstract boolean isMatched(SourceCode sourceCode);

    protected abstract Token createToken(String content);
}

class NumberTokenFactory extends TokenFactoryBase {

    @Override
    protected boolean isLeftTrim(char c) {
        return false;
    }

    @Override
    protected boolean isEnded(char c) {
        return Scanner.TOKEN_DELIMITER.contains(c);
    }

    @Override
    protected boolean isMatched(SourceCode sourceCode) {
        return Character.isDigit(sourceCode.getChar());
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
    @Override
    protected boolean isEnded(char c) {
        return Scanner.TOKEN_DELIMITER.contains(c);
    }

    @Override
    protected boolean isMatched(SourceCode sourceCode) {
        return sourceCode.getChar() == '.';
    }

    @Override
    protected Token createToken(String content) {
        if (content.isEmpty())
            throw new IllegalTokenContentException("property chain not finished");
        return propertyToken(content.split("\\."));
    }

    @Override
    protected boolean isLeftTrim(char c) {
        return c == '.' || Character.isWhitespace(c);
    }

}
