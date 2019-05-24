package com.github.leeonky.dal.token;

public abstract class TokenCandidate {
    private final int position;
    private final StringBuilder stringBuilder = new StringBuilder();

    public TokenCandidate(char c, int position) {
        this.position = position;
        if (!isDiscardFirstChar())
            stringBuilder.append(c);
    }

    @Deprecated
    public TokenCandidate(char c) {
        this(c, 0);
    }

    public static TokenCandidate createTokenCandidate(char c, int position) {
        if (NumberTokenCandidate.isBegin(c))
            return new NumberTokenCandidate(c);
        if (PropertyTokenCandidate.isBegin(c))
            return new PropertyTokenCandidate(c);
        if (ConstIndexTokenCandidate.isBegin(c))
            return new ConstIndexTokenCandidate(c, position);
        if (OperatorTokenCandidate.isBegin(c))
            return new OperatorTokenCandidate(c);
        if (BeginBracketTokenCandidate.isBegin(c))
            return new BeginBracketTokenCandidate(c);
        if (EndBracketTokenCandidate.isBegin(c))
            return new EndBracketTokenCandidate(c);
        if (SingleQuotationTokenCandidate.isBegin(c))
            return new SingleQuotationTokenCandidate(c);
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
        return Character.isWhitespace(c) || ConstIndexTokenCandidate.isBegin(c);
    }

    public boolean isIncludedLastChar(char c) {
        return false;
    }

    public boolean isDiscardedLastChar(char c) {
        return false;
    }

    public boolean isDiscardFirstChar() {
        return false;
    }

    public int getPosition() {
        return position;
    }
}
