package com.github.leeonky.dal.token;

class EndBracketTokenCandidateFactory implements TokenCandidateFactory {

    static final EndBracketTokenCandidateFactory INSTANCE = new EndBracketTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new TokenCandidate(sourceCode) {
            @Override
            protected Token toToken() {
                return Token.endBracketToken();
            }
        };
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return sourceCode.getChar() == ')';
    }
}
