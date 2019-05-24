package com.github.leeonky.dal.token;

public class SingleQuotationTokenCandidate extends TokenCandidate {
    public SingleQuotationTokenCandidate(char c) {
        super(c);
    }

    public static boolean isBegin(char c) {
        return c == '\'';
    }

    @Override
    public Token toToken() {
        return Token.stringToken(content());
    }

    @Override
    public boolean isDiscardedLastChar(char c) {
        return c == '\'';
    }

    @Override
    public boolean isDiscardFirstChar() {
        return true;
    }
}
