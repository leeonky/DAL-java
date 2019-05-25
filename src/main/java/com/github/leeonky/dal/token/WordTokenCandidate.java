package com.github.leeonky.dal.token;

import static com.github.leeonky.dal.token.Token.wordToken;

class WordTokenCandidate extends TokenCandidate {
    WordTokenCandidate(SourceCode sourceCode) {
        super(sourceCode);
    }

    @Override
    public Token toToken() {
        return wordToken(content());
    }

    @Override
    public boolean isNextTokenStart(char c) {
        return super.isNextTokenStart(c)
                || BeginBracketTokenCandidate.isBegin(c)
                || OperatorTokenCandidate.isBegin(c);
    }
}
