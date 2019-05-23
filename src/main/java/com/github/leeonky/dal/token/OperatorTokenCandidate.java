package com.github.leeonky.dal.token;

public class OperatorTokenCandidate extends TokenCandidate {
    public OperatorTokenCandidate(char c) {
        super(c);
    }

    public static boolean isStartOperator(char c) {
        switch (c) {
            case '=':
            case '>':
            case '<':
            case '+':
            case '-':
            case '*':
            case '/':
            case '&':
            case '|':
                return true;
        }
        return false;
    }

    @Override
    public Token toToken() {
        return Token.operatorToken(content());
    }
}
