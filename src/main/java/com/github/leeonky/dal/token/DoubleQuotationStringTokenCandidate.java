package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

class DoubleQuotationStringTokenCandidate extends TokenCandidate {
    private int codeLength = 0;

    private final EscapeFlag escapeFlag = new EscapeFlag('\\') {

        @Override
        protected void defaultAction(int c) {
            DoubleQuotationStringTokenCandidate.super.append(c);
        }

        @Override
        protected void triggerAction(int c) {
            DoubleQuotationStringTokenCandidate.super.append(getEscapedChar(c));
        }
    };

    DoubleQuotationStringTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    protected Token toToken() {
        if (!isFinished())
            throw new SyntaxException(getStartPosition() + codeLength + 1, "string should end with '\"'");
        return Token.constValueToken(content());
    }

    @Override
    protected boolean append(int c) {
        codeLength++;
        return escapeFlag.consume(c);
    }

    private char getEscapedChar(int c) {
        switch (c) {
            case '"':
                return '"';
            case 't':
                return '\t';
            case 'n':
                return '\n';
            case '\\':
                return '\\';
            default:
                throw new SyntaxException(getStartPosition() + codeLength, "unsupported escape char");
        }
    }

    @Override
    protected boolean needDiscardBeginChar() {
        return true;
    }

    @Override
    protected String discardedSuffix() {
        return "\"";
    }

    @Override
    protected boolean isUnexpectedChar(char c) {
        return false;
    }
}

class DoubleQuotationStringTokenCandidateFactory implements TokenCandidateFactory {

    static final DoubleQuotationStringTokenCandidateFactory INSTANCE = new DoubleQuotationStringTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new DoubleQuotationStringTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode, Token lastToken) {
        return sourceCode.getChar() == '"';
    }
}
