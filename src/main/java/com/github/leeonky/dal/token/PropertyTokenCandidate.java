package com.github.leeonky.dal.token;

public class PropertyTokenCandidate extends TokenCandidate {

    public PropertyTokenCandidate(char c) {
        super(c);
    }

    public static boolean isBegin(char c) {
        return c == '.';
    }

    @Override
    public Token toToken() {
        return Token.propertyToken(content());
    }

    @Override
    public boolean isExcludedSplitChar(char c) {
        return super.isExcludedSplitChar(c)
                || OperatorTokenCandidate.isBegin(c)
                || BeginBracketTokenCandidate.isBegin(c);
    }
}
