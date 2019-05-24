package com.github.leeonky.dal.token;

import java.math.BigDecimal;

public class NumberTokenCandidate extends TokenCandidate {
    public NumberTokenCandidate(char c) {
        super(c);
    }

    public static boolean isBegin(char c) {
        return Character.isDigit(c);
    }

    @Override
    public Token toToken() {
        return Token.numebrToken(new BigDecimal(content()));
    }

    @Override
    public boolean isExcludedSplitChar(char c) {
        return super.isExcludedSplitChar(c)
                || OperatorTokenCandidate.isBegin(c)
                || BeginBracketTokenCandidate.isBegin(c);
    }
}
