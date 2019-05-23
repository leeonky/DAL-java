package com.github.leeonky.dal.token;

public class ItemTokenCandidate extends TokenCandidate {
    public ItemTokenCandidate(char c) {
        super(c);
    }

    @Override
    public Token toToken() {
        return Token.itemToken(content());
    }

    @Override
    public boolean isIncludedLastChar(char c) {
        return c == ']';
    }
}
