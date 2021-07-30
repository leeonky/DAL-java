package com.github.leeonky.dal.token;

class PropertyTokenCandidate extends TokenCandidate {
    PropertyTokenCandidate(SourceCode sourceCode) {
        super(sourceCode, Scanner.CHAR_SPLIT);
    }

    @Override
    protected Token toToken() {
        return Token.propertyToken(content().split("\\."));
    }

    @Override
    protected boolean needDiscardBeginChar() {
        return true;
    }

    @Override
    protected boolean isDiscardedPrefix(char c) {
        return Character.isWhitespace(c);
    }
}

class PropertyTokenCandidateFactory implements TokenCandidateFactory {

    static final PropertyTokenCandidateFactory INSTANCE = new PropertyTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new PropertyTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode, Token lastToken) {
        return sourceCode.getChar() == '.';
    }

}

