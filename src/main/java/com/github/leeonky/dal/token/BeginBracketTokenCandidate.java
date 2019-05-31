package com.github.leeonky.dal.token;

class BeginBracketTokenCandidate extends TokenCandidate {
    BeginBracketTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    protected Token toToken() {
        return Token.beginBracketToken();
    }

    @Override
    protected boolean isUnexpectedChar(char c) {
        return true;
    }
}

class BeginBracketTokenCandidateFactory implements TokenCandidateFactory {

    static final BeginBracketTokenCandidateFactory INSTANCE = new BeginBracketTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new BeginBracketTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return sourceCode.getChar() == '(';
    }

}
