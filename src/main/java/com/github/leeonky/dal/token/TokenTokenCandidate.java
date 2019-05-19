package com.github.leeonky.dal.token;

import static com.github.leeonky.dal.token.Token.token;

public class TokenTokenCandidate extends TokenCandidate {
    public TokenTokenCandidate(char c) {
        super(c);
    }

    @Override
    public Token toToken() {
        return token(content());
    }
}
