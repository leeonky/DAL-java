package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.BracketNode;

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

    public boolean isCurrentBeginBracket() {
        return currentType() == Token.Type.BEGIN_BRACKET;
    }

    public boolean isCurrentEndBracketAndTakeThenFinishBracket(BracketNode bracketNode) {
        if (currentType() == Token.Type.END_BRACKET) {
            if (bracketNode == null)
                throw new SyntaxException(index, "missed begin bracket");
            bracketNode.finishBracket();
            index++;
            return true;
        }
        return false;
    }
}
