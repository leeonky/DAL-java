package com.github.leeonky.dal.token;

public class BeginBracketTokenCandidate extends TokenCandidate {
    public BeginBracketTokenCandidate(char c) {
        super(c);
    }

    @Override
    public Token toToken() {
        return Token.beginBrachetToken();
    }
}
