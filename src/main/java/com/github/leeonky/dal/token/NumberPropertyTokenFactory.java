package com.github.leeonky.dal.token;

import java.math.BigDecimal;

abstract class NumberPropertyTokenFactory implements TokenFactory {
    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        if (sourceCode.notEnd() && matches(sourceCode.getChar(), previous))
            return parseConstValueToken(sourceCode);
        return null;
    }

    protected abstract boolean matches(char c, Token previous);

    private Token parseConstValueToken(SourceCode sourceCode) {
        int startPosition = sourceCode.getPosition();
        TokenParser parser = createParser();
        int codeLength = 0;
        while (sourceCode.notEnd() && parser.feed(sourceCode.takeChar()))
            codeLength++;
        if (parser.isFinished())
            sourceCode.seek(-1);
//        if (!parser.isFinished())
//            throw new SyntaxException(startPosition + codeLength, errorMessage);
        return createToken(parser.value());
    }

    protected abstract Token createToken(String value);

    protected abstract TokenParser createParser();
}

class NumberTokenFactory extends NumberPropertyTokenFactory {

    @Override
    protected Token createToken(String value) {
        return Token.constValueToken(getNumber(value));
    }

    @Override
    protected TokenParser createParser() {
        return new NumberParser();
    }

    private Number getNumber(String value) {
        try {
            return BigDecimal.valueOf(Long.decode(value));
        } catch (NumberFormatException ignore) {
            return new BigDecimal(value);
        }
    }

    @Override
    protected boolean matches(char c, Token previous) {
        return Character.isDigit(c);
    }
}

class OperatorTokenFactory extends NumberPropertyTokenFactory {

    @Override
    protected Token createToken(String value) {
        return Token.operatorToken(value);
    }

    @Override
    protected TokenParser createParser() {
        return new OperatorParser();
    }

    @Override
    protected boolean matches(char c, Token previous) {
        return Scanner.OPERATOR_CHAR.contains(c) && (!(c == '/' && previous != null && previous.isOperatorMatches()));
    }
}
