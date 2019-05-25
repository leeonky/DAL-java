package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntexException;

class DoubleQuotationStringTokenCandidate extends TokenCandidate {

    DoubleQuotationStringTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    public Token toToken() {
        if (!isFinished())
            throw new SyntexException(getStartPosition() + content().length() + 1, "string should end with '\"'");
        return Token.stringToken(content());
    }

    @Override
    public boolean isDiscardBeginChar() {
        return true;
    }

    @Override
    protected String discardedSuffix() {
        return "\"";
    }
}

class DoubleQuotationStringTokenCandidateFactory implements TokenCandidateFactory {

    static final DoubleQuotationStringTokenCandidateFactory INSTANCE = new DoubleQuotationStringTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new DoubleQuotationStringTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return sourceCode.getChar() == '"';
    }
}
