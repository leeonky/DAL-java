package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;

import java.util.function.Function;

import static java.util.Optional.ofNullable;

public class TokenContentInToken {
    private Function<TokenParser, Token> tokenGetter = context -> null;

    private TokenContentInToken() {
    }

    public static TokenContentInToken byFactory(TokenFactory factory) {
        return new TokenContentInToken().setTokenGetter(factory::fetchToken);
    }

    public TokenContentInToken or(TokenFactory tokenFactory) {
        return copy().setTokenGetter(context -> ofNullable(tokenGetter.apply(context))
                .orElseGet(() -> tokenFactory.fetchToken(context)));
    }

    Token getToken(TokenParser context) {
        return tokenGetter.apply(context);
    }

    protected TokenContentInToken copy() {
        return new TokenContentInToken().setTokenGetter(tokenGetter);
    }

    private TokenContentInToken setTokenGetter(Function<TokenParser, Token> tokenGetter) {
        this.tokenGetter = tokenGetter;
        return this;
    }
}
