package com.github.leeonky.dal.token;

import java.util.List;

import static java.util.Arrays.asList;

public class OperatorTokenCandidate extends TokenCandidate {
    private static final List<String> MULTI_CHAR_OPTS = asList(">=", "<=", "&&", "||", "!=");

    public OperatorTokenCandidate(char c) {
        super(c);
    }

    public static boolean isBegin(char c) {
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
            case '!':
                return true;
        }
        return false;
    }

    @Override
    public Token toToken() {
        return Token.operatorToken(content());
    }

    @Override
    public boolean isExcludedSplitChar(char c) {
        String operatorCandidate = content() + c;
        return !MULTI_CHAR_OPTS.stream().anyMatch(opt -> opt.startsWith(operatorCandidate));
    }

    @Override
    public boolean isIncludedLastChar(char c) {
        return true; //Every operator has 2 chars at most, so directly return true
    }
}
