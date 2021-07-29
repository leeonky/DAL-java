package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

class AccessElementTokenCandidate extends TokenCandidate {

    AccessElementTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    protected Token toToken() {
        if (!isFinished())
            throw new SyntaxException(getStartPosition() + content().length() + 1, "missed ']'");
        try {
            return Token.constIndexToken(Integer.valueOf(content()));
        } catch (NumberFormatException ignore) {
            return Token.propertyToken(content());
        }
    }

    @Override
    protected String discardedSuffix() {
        return "]";
    }

    @Override
    protected boolean isDiscardBeginChar() {
        return true;
    }

    @Override
    protected boolean isUnexpectedChar(char c) {
        return false;
    }
}

class AccessElementTokenCandidateFactory implements TokenCandidateFactory {

    static final AccessElementTokenCandidateFactory INSTANCE = new AccessElementTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new AccessElementTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode, Token lastToken) {
        return sourceCode.getChar() == '[';
    }
}
