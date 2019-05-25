package com.github.leeonky.dal.token;

class PropertyTokenCandidate extends TokenCandidate {

    PropertyTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    public Token toToken() {
        return Token.propertyToken(content());
    }

    @Override
    public boolean isExcludedSplitChar(char c) {
        return super.isExcludedSplitChar(c)
                || OperatorTokenCandidate.isBegin(c)
                || BeginBracketTokenCandidate.isBegin(c);
    }
}

class PropertyTokenCandidateFactory implements TokenCandidateFactory {

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new PropertyTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return sourceCode.getChar() == '.';
    }
}
