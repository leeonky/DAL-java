package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

class RegexTokenCandidate extends TokenCandidate {
    private boolean isEscape = false;
    private int codeLength = 0;

    RegexTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    protected Token toToken() {
        if (!isFinished())
            //TODO missing UT
            throw new SyntaxException(getStartPosition() + codeLength + 1, "regex should end with '/'");
        return Token.regexToken(content());
    }

    @Override
    protected boolean append(char c) {
        codeLength++;
        if (isEscape) {
            super.append(getEscapedChar(c));
            isEscape = false;
        } else {
            if (c == '\\') {
                isEscape = true;
                return true;
            }
            super.append(c);
        }
        return false;
    }

    private char getEscapedChar(char c) {
        //TODO missing UT
        switch (c) {
            case '/':
                return '/';
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
    protected boolean isDiscardBeginChar() {
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
