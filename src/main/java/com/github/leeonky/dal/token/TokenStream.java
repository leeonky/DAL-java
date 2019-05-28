package com.github.leeonky.dal.token;

import java.util.ArrayList;
import java.util.List;

public class TokenStream {
    private final List<Token> tokens = new ArrayList<>();
    private int index = 0;

    public Token pop() {
        return tokens.get(index++);
    }

    public boolean hasTokens() {
        return index < tokens.size();
    }

    public List<Token> allTokens() {
        return new ArrayList<>(tokens);
    }

    public void appendToken(Token token) {
        tokens.add(token);
    }

    public Token.Type currentType() {
        return tokens.get(index).getType();
    }

    public boolean isCurrentSingleEvaluateNode() {
        return currentType() == Token.Type.PROPERTY;
    }

    public int getPosition() {
        return hasTokens() ? tokens.get(index).getPositionBegin() : (index > 0 ? tokens.get(index - 1).getPositionEnd() : 0);
    }
}
