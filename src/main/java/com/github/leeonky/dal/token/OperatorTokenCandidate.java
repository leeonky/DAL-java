package com.github.leeonky.dal.token;

import java.util.List;

import static java.util.Arrays.asList;

class OperatorTokenCandidate extends TokenCandidate {
    private static final List<String> MULTI_CHAR_OPTS = asList(">=", "<=", "&&", "||", "!=");

    OperatorTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    static boolean isBegin(char c) {
        switch (c) {
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

    @Override
    public Token toToken() {
        return Token.operatorToken(content());
    }

    @Override
    public boolean isNextTokenStart(char c) {
        String operatorCandidate = content() + c;
        return !MULTI_CHAR_OPTS.stream().anyMatch(opt -> opt.startsWith(operatorCandidate));
    }

}

class OperatorTokenCandidateFactory implements TokenCandidateFactory {
    public static final OperatorTokenCandidateFactory INSTANCE = new OperatorTokenCandidateFactory();

    @Override
    public TokenCandidate createTokenCandidate(SourceCode sourceCode) {
        return new OperatorTokenCandidate(sourceCode);
    }

    @Override
    public boolean isBegin(SourceCode sourceCode) {
        return OperatorTokenCandidate.isBegin(sourceCode.getChar());
    }
}

