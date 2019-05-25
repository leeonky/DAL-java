package com.github.leeonky.dal.token;

class EndBracketTokenCandidate extends TokenCandidate {
    EndBracketTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    static boolean isBegin(char c) {
        return c == ')';
    }

    @Override
    public Token toToken() {
        return Token.endBrachetToken();
    }
}

class EndBracketTokenCandidateFactory implements TokenCandidateFactory {

    public static final EndBracketTokenCandidateFactory INSTANCE = new EndBracketTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new EndBracketTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return EndBracketTokenCandidate.isBegin(sourceCode.getChar());
    }
}
