package com.github.leeonky.dal.token;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class TokenStream {
    public final LinkedList<Token> tokens = new LinkedList<>();

    public TokenStream(List<Token> tokens) {
        this.tokens.addAll(tokens);
    }

    public TokenStream() {
    }

    public Token pop() {
        return tokens.pop();
    }

    public boolean hasTokens() {
        return tokens.size() > 0;
    }

    public boolean matchAndTakeKeyWord(String keyword) {
        if (hasTokens() && tokens.getFirst().isWord(keyword)) {
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
}
