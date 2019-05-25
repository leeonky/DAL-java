package com.github.leeonky.dal.token;

import static com.github.leeonky.dal.token.Token.wordToken;

class WordTokenCandidate extends TokenCandidate {
    WordTokenCandidate(SourceCode sourceCode) {
        super(sourceCode, Scanner.CHAR_SPLIT);
    }

    @Override
    public Token toToken() {
        return wordToken(content());
    }
}
