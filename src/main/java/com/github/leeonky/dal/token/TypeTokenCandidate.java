package com.github.leeonky.dal.token;

import static com.github.leeonky.dal.token.Token.typeToken;

public class TypeTokenCandidate extends TokenCandidate {
    public TypeTokenCandidate(char c) {
        super(c);
    }

    @Override
    public Token toToken() {
        return typeToken(content());
    }
}
