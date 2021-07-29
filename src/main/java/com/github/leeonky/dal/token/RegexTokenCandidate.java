package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

class RegexTokenCandidate extends TokenCandidate {
    private int codeLength = 0;
    private final EscapeFlag escapeFlag = new EscapeFlag('\\') {

        @Override
        protected void defaultAction(int c) {
            RegexTokenCandidate.super.append(c);
        }

        @Override
        protected void triggerAction(int c) {
            getEscapedChar(c).chars().forEach(RegexTokenCandidate.super::append);
        }
    };

    RegexTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    protected Token toToken() {
        if (!isFinished())
            throw new SyntaxException(getStartPosition() + codeLength + 1, "regex should end with '/'");
        return Token.regexToken(content());
    }

    @Override
    protected boolean append(int c) {
        codeLength++;
        return escapeFlag.consume(c);
    }

    private String getEscapedChar(int c) {
        switch (c) {
            case '/':
                return "/";
            case 't':
                return "\t";
            case 'n':
                return "\n";
            case '\\':
                return "\\\\";
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
        return "/";
    }

    @Override
    protected boolean isUnexpectedChar(char c) {
        return false;
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
