package com.github.leeonky.dal.token;

public class PropertyTokenCandidate extends TokenCandidate {

    public PropertyTokenCandidate(char c) {
        super(c);
    }

    @Override
    public Token toToken() {
        return Token.propertyToken(content());
    }
}
