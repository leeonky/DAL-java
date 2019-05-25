package com.github.leeonky.dal.token;

class OperatorTokenCandidate extends TokenCandidate {

    OperatorTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    public Token toToken() {
        return Token.operatorToken(content());
    }

    @Override
    public boolean isUnexpectedChar(char c) {
        String operatorCandidate = content() + c;
        return !Scanner.MULTI_CHAR_OPTS.stream().anyMatch(opt -> opt.startsWith(operatorCandidate));
    }

}

class OperatorTokenCandidateFactory implements TokenCandidateFactory {
    static final OperatorTokenCandidateFactory INSTANCE = new OperatorTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new OperatorTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        switch (sourceCode.getChar()) {
            case '=':
            case '>':
            case '<':
            case '+':
            case '-':
            case '*':
            case '/':
            case '&':
            case '|':
            case '!':
                return true;
        }
        return false;
    }
}

