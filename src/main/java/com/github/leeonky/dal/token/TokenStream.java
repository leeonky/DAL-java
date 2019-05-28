package com.github.leeonky.dal.token;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class TokenStream {
    private final LinkedList<Token> tokens = new LinkedList<>();

    public Token pop() {
        return tokens.pop();
    }

    public boolean hasTokens() {
        return tokens.size() > 0;
    }

    public List<Token> allTokens() {
        return new ArrayList<>(tokens);
    }

    public void appendToken(Token token) {
        tokens.add(token);
    }

    public Token.Type currentType() {
        return tokens.getFirst().getType();
    }

    public boolean isCurrentSingleEvaluateNode() {
        return currentType() == Token.Type.PROPERTY;
    }
}
