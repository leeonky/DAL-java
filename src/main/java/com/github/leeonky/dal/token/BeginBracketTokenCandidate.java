package com.github.leeonky.dal.token;

class BeginBracketTokenCandidateFactory implements TokenCandidateFactory {

    static final BeginBracketTokenCandidateFactory INSTANCE = new BeginBracketTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new TokenCandidate(sourceCode) {
            @Override
            protected Token toToken() {
                return Token.beginBracketToken();
            }
        };
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return sourceCode.getChar() == '(';
    }
}
