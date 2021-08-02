package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

abstract class NumberPropertyTokenFactory implements TokenFactory {
    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        if (sourceCode.notEnd() && matches(sourceCode.getChar(), previous))
            return parseConstValueToken(sourceCode);
        return null;
    }

    protected abstract boolean matches(char c, Token previous);

    protected Token parseConstValueToken(SourceCode sourceCode) {
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

class PropertyChainTokenFactory extends NumberPropertyTokenFactory {

    @Override
    protected Token parseConstValueToken(SourceCode sourceCode) {
        int startPosition = sourceCode.getPosition();
        TokenParser parser = createParser();
        int codeLength = 0;
        while (sourceCode.notEnd() && parser.feed(sourceCode.takeChar()))
            codeLength++;
        if (parser.isFinished())
            sourceCode.seek(-1);
        if (!parser.canFinish())
            throw new SyntaxException(startPosition + codeLength, "property chain not finished");
        return createToken(parser.value());
    }

    @Override
    protected boolean matches(char c, Token previous) {
        return '.' == c;
    }

    @Override
    protected Token createToken(String value) {
        return Token.propertyToken(value.split("\\."));
    }

    @Override
    protected TokenParser createParser() {
        return new PropertyChainParser();
    }
}
