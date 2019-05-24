package com.github.leeonky.dal.token;

public class EndBracketTokenCandidate extends TokenCandidate {
    public EndBracketTokenCandidate(char c) {
        super(c);
    }

    @Override
    public Token toToken() {
        return Token.endBrachetToken();
    }
}
