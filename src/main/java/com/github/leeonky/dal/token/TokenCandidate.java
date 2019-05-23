package com.github.leeonky.dal.token;

public abstract class TokenCandidate {
    private StringBuilder stringBuilder = new StringBuilder();

    public TokenCandidate(char c) {
        stringBuilder.append(c);
    }

    public static TokenCandidate createTokenCandidate(char c) {
        if (Character.isDigit(c))
            return new NumberTokenCandidate(c);
        if (c == '.')
            return new PropertyTokenCandidate(c);
        if (c == '[')
            return new ItemTokenCandidate(c);
        if (OperatorTokenCandidate.isOperator(c))
            return new OperatorTokenCandidate(c);
        return new WordTokenCandidate(c);
    }

    public void append(char c) {
        stringBuilder.append(c);
    }

    public String content() {
        return stringBuilder.toString();
    }

    public abstract Token toToken();

    public boolean isExcludedSplitChar(char c) {
        return Character.isWhitespace(c) || c == '[';
    }

    public boolean isIncludedLastChar(char c) {
        return false;
    }
}
