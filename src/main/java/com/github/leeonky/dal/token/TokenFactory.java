package com.github.leeonky.dal.token;

import java.math.BigDecimal;

import static com.github.leeonky.dal.token.Token.constValueToken;

public interface TokenFactory {
    Token fetchToken(SourceCode sourceCode, Token previous);
}

class NumberTokenFactory extends TokenFactoryBase {

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
