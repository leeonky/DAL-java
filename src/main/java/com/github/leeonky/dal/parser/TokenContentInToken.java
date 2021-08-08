package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;

import java.util.function.Function;

//TODO missing UT
public class TokenContentInToken extends ContentPreprocessor<Token, TokenContentInToken> {
    private Function<ParsingContext, Token> tokenGetter = context -> null;

    private TokenContentInToken() {
        super();
    }

    public static TokenContentInToken byFactories(TokenFactory factory) {
        return new TokenContentInToken().setTokenGetter(factory::fetchToken);
    }

    protected TokenContentInToken setTokenGetter(Function<ParsingContext, Token> tokenGetter) {
        this.tokenGetter = tokenGetter;
        return this;
    }

    Token getToken(ParsingContext context) {
        return tokenGetter.apply(context);
    }

    @Override
    protected TokenContentInToken newInstance() {
        return new TokenContentInToken();
    }

    @Override
    protected TokenContentInToken copy() {
        return super.copy().setTokenGetter(tokenGetter);
    }
}
