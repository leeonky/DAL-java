package com.github.leeonky.dal.token;

import static com.github.leeonky.dal.DALCompiler.*;
import static com.github.leeonky.dal.token.Token.*;

class WordTokenCandidate extends TokenCandidate {
    WordTokenCandidate(SourceCode sourceCode) {
        super(sourceCode, Scanner.CHAR_SPLIT);
    }

    @Override
    protected Token toToken() {
        String content = content();
        if (NULL.equals(content))
            return constValueToken(null);
        if (TRUE.equals(content))
            return constValueToken(true);
        if (FALSE.equals(content))
            return constValueToken(false);
        if (AND.equals(content))
            return operatorToken("&&");
        if (OR.equals(content))
            return operatorToken("||");
        if (MATCHES.equals(content))
            return operatorToken("matches");
        if (Scanner.KEYWORD_SETS.contains(content))
            return keyWordToken(content);
        return wordToken(content);
    }
}
