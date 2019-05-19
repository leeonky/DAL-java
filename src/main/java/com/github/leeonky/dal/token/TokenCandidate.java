package com.github.leeonky.dal.token;

public abstract class TokenCandidate {
    private StringBuilder stringBuilder = new StringBuilder();

    public TokenCandidate(char c) {
        stringBuilder.append(c);
    }

    public static TokenCandidate createTokenCandidate(char c) {
        if (Character.isDigit(c))
            return new NumberTokenCandidate(c);
        return new TokenTokenCandidate(c);
    }

    public void append(char c) {
        stringBuilder.append(c);
    }

    public String content() {
        return stringBuilder.toString();
    }

    public abstract Token toToken();

    public boolean canEnd(char c) {
        return Character.isWhitespace(c);
    }
}
