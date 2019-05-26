package com.github.leeonky.dal.token;

import java.math.BigDecimal;

class NumberTokenCandidate extends TokenCandidate {

    NumberTokenCandidate(SourceCode sourceCode) {
        super(sourceCode, Scanner.CHAR_SPLIT);
    }

    @Override
    protected Token toToken() {
        return Token.numberToken(new BigDecimal(content()));
    }
}

class NumberTokenCandidateFactory implements TokenCandidateFactory {
    static final NumberTokenCandidateFactory INSTANCE = new NumberTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new NumberTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return Character.isDigit(sourceCode.getChar());
    }
}

