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
        return Character.isSpaceChar(c)
                || OperatorTokenCandidate.isBegin(c)
                || ItemTokenCandidate.isBegin(c)
                ;
    }
}
