package com.github.leeonky.dal.token;

class EndBracketTokenCandidate extends TokenCandidate {
    EndBracketTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    protected Token toToken() {
        return Token.endBracketToken();
    }

    @Override
    protected boolean isUnexpectedChar(char c) {
        return true;
    }
}

class EndBracketTokenCandidateFactory implements TokenCandidateFactory {

    static final EndBracketTokenCandidateFactory INSTANCE = new EndBracketTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new EndBracketTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return sourceCode.getChar() == ')';
    }
}
