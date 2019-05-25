package com.github.leeonky.dal.token;

class BeginBracketTokenCandidate extends TokenCandidate {
    BeginBracketTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    static boolean isBegin(char c) {
        return c == '(';
    }

    @Override
    public Token toToken() {
        return Token.beginBrachetToken();
    }
}

class BeginBracketTokenCandidateFactory implements TokenCandidateFactory {

    public static final BeginBracketTokenCandidateFactory INSTANCE = new BeginBracketTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new BeginBracketTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return BeginBracketTokenCandidate.isBegin(sourceCode.getChar());
    }
}
