package com.github.leeonky.dal.token;

class PropertyTokenCandidateFactory implements TokenCandidateFactory {

    static final PropertyTokenCandidateFactory INSTANCE = new PropertyTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new TokenCandidate(sourceCode, Scanner.CHAR_SPLIT) {
            @Override
            public Token toToken() {
                return Token.propertyToken(content());
            }
        };
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return sourceCode.getChar() == '.';
    }
}
