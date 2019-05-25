package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntexException;

class ConstIndexTokenCandidate extends TokenCandidate {

    ConstIndexTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    public Token toToken() {
        if (!isFinished())
            throw new SyntexException(getStartPosition() + content().length() + 1, "missed ']'");
        int value;
        try {
            value = Integer.valueOf(content());
        } catch (NumberFormatException e) {
            throw new SyntexException(getStartPosition() + 1, "only support const int array index");
        }
        return Token.constIndexToken(value);
    }

    @Override
    protected String discardedSuffix() {
        return "]";
    }

    @Override
    public boolean isDiscardBeginChar() {
        return true;
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
