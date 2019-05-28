package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

class ConstIndexTokenCandidate extends TokenCandidate {

    ConstIndexTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    protected Token toToken() {
        if (!isFinished())
            throw new SyntaxException(getStartPosition() + content().length() + 1, "missed ']'");
        try {
            return Token.constIndexToken(Integer.valueOf(content()));
        } catch (NumberFormatException e) {
            throw new SyntaxException(getStartPosition() + 1, "only support const int array index");
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

class ConstIndexTokenCandidateFactory implements TokenCandidateFactory {

    static final ConstIndexTokenCandidateFactory INSTANCE = new ConstIndexTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new ConstIndexTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return sourceCode.getChar() == '[';
    }
}
