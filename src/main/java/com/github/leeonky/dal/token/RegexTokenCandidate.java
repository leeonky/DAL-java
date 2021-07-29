package com.github.leeonky.dal.token;

class RegexTokenCandidate extends TokenCandidate {

    public RegexTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    protected Token toToken() {
        return Token.regexToken("1");
    }

    //TODO
    @Override
    protected boolean isUnexpectedChar(char c) {
        return super.isUnexpectedChar(c);
    }
}

class RegexTokenCandidateFactory implements TokenCandidateFactory {

    static final RegexTokenCandidateFactory INSTANCE = new RegexTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new RegexTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode, Token lastToken) {
        return sourceCode.getChar() == '/' && lastToken != null && lastToken.isOperatorMatches();
    }
}
