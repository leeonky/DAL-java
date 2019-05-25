package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntexException;

public class SingleQuotationTokenCandidate extends TokenCandidate {
    private boolean finished = false;

    public SingleQuotationTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    public static boolean isBegin(char c) {
        return c == '\'';
    }

    @Override
    public Token toToken() {
        if (!finished)
            throw new SyntexException(getStartPosition() + content().length() + 1, "string should end with '\''");
        return Token.stringToken(content());
    }

    @Override
    public boolean isDiscardedLastChar(char c) {
        return c == '\'' && (finished = true);
    }

    @Override
    public boolean isDiscardPrefix() {
        return true;
    }
}

class SingleQuotationTokenCandidateFactory implements TokenCandidateFactory {

    public static final SingleQuotationTokenCandidateFactory INSTANCE = new SingleQuotationTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new SingleQuotationTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return SingleQuotationTokenCandidate.isBegin(sourceCode.getChar());
    }
}
