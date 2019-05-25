package com.github.leeonky.dal.token;

import java.math.BigDecimal;

class NumberTokenCandidate extends TokenCandidate {
    NumberTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    public Token toToken() {
        return Token.numebrToken(new BigDecimal(content()));
    }

    @Override
    public boolean isNextTokenStart(char c) {
        return super.isNextTokenStart(c)
                || OperatorTokenCandidate.isBegin(c)
                || BeginBracketTokenCandidate.isBegin(c);
    }
}

class NumberTokenCandidateFactory implements TokenCandidateFactory {
    public static final NumberTokenCandidateFactory INSTANCE = new NumberTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new NumberTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return Character.isDigit(sourceCode.getChar());
    }
}

