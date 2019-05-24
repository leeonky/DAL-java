package com.github.leeonky.dal.token;

public class SingleQuotationTokenCandidate extends TokenCandidate {
    private StringBuilder stringContent = new StringBuilder();

    public SingleQuotationTokenCandidate(char c) {
        super(c);
    }

    public static boolean isBegin(char c) {
        return c == '\'';
    }

    @Override
    public void append(char c) {
        stringContent.append(c);
    }

    @Override
    public Token toToken() {
        return Token.stringToken(stringContent.toString());
    }

    @Override
    public boolean isDiscardedLastChar(char c) {
        return c == '\'';
    }
}
