package com.github.leeonky.dal.token;

public abstract class NumberPropertyTokenFactory implements TokenFactory {
    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        if (sourceCode.notEnd() && Character.isDigit(sourceCode.getChar()))
            return parseConstValueToken(sourceCode);
        return null;
    }

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
