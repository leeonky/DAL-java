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

    public boolean matchAndTakeKeyWord(String keyword) {
        if (hasTokens() && tokens.getFirst().isKeyWord(keyword)) {
            tokens.pop();
            return true;
        }
        return false;
    }

    public boolean matchAndTakeRootValue() {
        if (hasTokens() && tokens.getFirst().getType() == Token.Type.ROOT_VALUE) {
            tokens.pop();
            return true;
        }
        return false;
    }

    public List<Token> allTokens() {
        return new ArrayList<>(tokens);
    }

    public void appendToken(Token token) {
        tokens.add(token);
    }

    public void insertFirst(Token token) {
        tokens.addFirst(token);
    }
}
