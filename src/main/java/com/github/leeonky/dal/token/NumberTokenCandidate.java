package com.github.leeonky.dal.token;

import java.math.BigDecimal;

public class NumberTokenCandidate extends TokenCandidate {
    public NumberTokenCandidate(char c) {
        super(c);
    }

    @Override
    public Token toToken() {
        return Token.numebrToken(new BigDecimal(content()));
    }

    @Override
    public boolean isExcludedSplitChar(char c) {
        return Character.isSpaceChar(c) || OperatorTokenCandidate.isStartOperator(c);
    }
}
