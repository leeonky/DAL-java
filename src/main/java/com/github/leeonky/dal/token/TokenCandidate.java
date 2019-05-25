package com.github.leeonky.dal.token;

public abstract class TokenCandidate {
    private final int startPosition;
    private final StringBuilder stringBuilder = new StringBuilder();

    public TokenCandidate(SourceCode sourceCode) {
        startPosition = sourceCode.getPosition();
        char c = sourceCode.takeChar();
        if (!isDiscardPrefix())
            stringBuilder.append(c);
    }

    protected void append(char c) {
        stringBuilder.append(c);
    }

    protected String content() {
        return stringBuilder.toString();
    }

    protected abstract Token toToken();

    protected boolean isNextTokenStart(char c) {
        return Character.isWhitespace(c) || c == '[';
    }

    protected boolean isNextTokenStart(SourceCode sourceCode) {
        return isNextTokenStart(sourceCode.getChar());
    }

    protected boolean isDiscardedLastChar(char c) {
        return false;
    }

    protected boolean isDiscardedSuffix(SourceCode sourceCode) {
        boolean discardedLastChar = isDiscardedLastChar(sourceCode.getChar());
        if (discardedLastChar)
            sourceCode.takeChar();
        return discardedLastChar;
    }

    protected boolean isDiscardPrefix() {
        return false;
    }

    protected int getStartPosition() {
        return startPosition;
    }

    Token getToken(SourceCode sourceCode) {
        while (sourceCode.hasContent()
                && !isDiscardedSuffix(sourceCode)
                && !isNextTokenStart(sourceCode))
            append(sourceCode.takeChar());
        return toToken();
    }
}
