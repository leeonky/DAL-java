package com.github.leeonky.dal.token;

public class ConstIndexTokenCandidate extends TokenCandidate {
    public ConstIndexTokenCandidate(char c) {
        super(c);
    }

    public static boolean isBegin(char c) {
        return c == '[';
    }

    @Override
    public Token toToken() {
        return Token.constIndexToken(Integer.valueOf(content()));
    }

    @Override
    public boolean isDiscardedLastChar(char c) {
        return c == ']';
    }

    @Override
    public boolean isDiscardFirstChar() {
        return true;
    }
}
