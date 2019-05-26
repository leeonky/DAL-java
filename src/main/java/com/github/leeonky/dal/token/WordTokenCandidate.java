package com.github.leeonky.dal.token;

import static com.github.leeonky.dal.DALCompiler.NULL;
import static com.github.leeonky.dal.token.Token.*;

class WordTokenCandidate extends TokenCandidate {
    WordTokenCandidate(SourceCode sourceCode) {
        super(sourceCode, Scanner.CHAR_SPLIT);
    }

    @Override
    protected Token toToken() {
        String content = content();
        if (NULL.equals(content))
            return nullValueToken();
        if (Scanner.KEYWORD_SETS.contains(content))
            return keyWordToken(content);
        return wordToken(content);
    }
}
