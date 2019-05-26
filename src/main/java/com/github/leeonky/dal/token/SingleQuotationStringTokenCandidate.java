package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntexException;

class SingleQuotationStringTokenCandidate extends TokenCandidate {

    SingleQuotationStringTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    protected Token toToken() {
        if (!isFinished())
            throw new SyntexException(getStartPosition() + content().length() + 1, "string should end with '\''");
        return Token.stringToken(content());
    }

    @Override
    protected boolean isDiscardBeginChar() {
        return true;
    }

    @Override
    protected String discardedSuffix() {
        return "'";
    }

    @Override
    protected boolean isUnexpectedChar(char c) {
        return false;
    }
}

class SingleQuotationStringTokenCandidateFactory implements TokenCandidateFactory {

    static final SingleQuotationStringTokenCandidateFactory INSTANCE = new SingleQuotationStringTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new SingleQuotationStringTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return sourceCode.getChar() == '\'';
    }
}
