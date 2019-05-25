package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntexException;

public class SingleQuotationTokenCandidate extends TokenCandidate {

    SingleQuotationTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    public Token toToken() {
        if (!isFinished())
            throw new SyntexException(getStartPosition() + content().length() + 1, "string should end with '\''");
        return Token.stringToken(content());
    }

    @Override
    public boolean isDiscardBeginChar() {
        return true;
    }

    @Override
    protected String discardedSuffix() {
        return "'";
    }
}

class SingleQuotationTokenCandidateFactory implements TokenCandidateFactory {

    static final SingleQuotationTokenCandidateFactory INSTANCE = new SingleQuotationTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new SingleQuotationTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return sourceCode.getChar() == '\'';
    }
}
