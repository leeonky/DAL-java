package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;

import java.util.function.Function;

import static java.util.Optional.ofNullable;

public class TokenContentInToken {
    private Function<TokenParser, Token> tokenGetter = parser -> null;

    private TokenContentInToken() {
    }

    public static TokenContentInToken byFactory(TokenFactory factory) {
        return new TokenContentInToken().setTokenGetter(factory::fetchToken);
    }

    public TokenContentInToken or(TokenFactory tokenFactory) {
        return copy().setTokenGetter(parser -> ofNullable(tokenGetter.apply(parser))
                .orElseGet(() -> tokenFactory.fetchToken(parser)));
    }

    Token getToken(TokenParser parser) {
        return tokenGetter.apply(parser);
    }

    protected TokenContentInToken copy() {
        return new TokenContentInToken().setTokenGetter(tokenGetter);
    }

    private TokenContentInToken setTokenGetter(Function<TokenParser, Token> tokenGetter) {
        this.tokenGetter = tokenGetter;
        return this;
    }
}