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

    public abstract Token toToken();

    public boolean isExcludedSplitChar(char c) {
        return Character.isWhitespace(c) || c == '[';
    }

    public boolean isIncludedLastChar(char c) {
        return false;
    }

    public boolean isDiscardedLastChar(char c) {
        return false;
    }

    public boolean isDiscardPrefix() {
        return false;
    }

    protected int getStartPosition() {
        return startPosition;
    }

    Token getToken(SourceCode sourceCode) {
        while (!sourceCode.isEnd()) {
            char c = sourceCode.getChar();
            if (isDiscardedLastChar(c)) {
                sourceCode.takeChar();
                break;
            }
            if (isExcludedSplitChar(c))
                break;
            append(c);
            sourceCode.takeChar();
            if (isIncludedLastChar(c))
                break;
        }
        return toToken();
    }
}
