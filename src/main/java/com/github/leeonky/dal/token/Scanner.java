package com.github.leeonky.dal.token;

import java.util.ArrayList;
import java.util.List;

public class Scanner {
    public List<Token> scan(SourceCode sourceCode) {
        List<Token> tokens = new ArrayList<>();
        while (!sourceCode.isEnd()) {
            tokens.add(TokenCandidateFactory.FACTORIES.stream()
                    .filter(f -> f.isBegin(sourceCode))
                    .map(f -> f.createTokenCandidate(sourceCode))
                    .findFirst().orElseGet(() -> new WordTokenCandidate(sourceCode))
                    .getToken(sourceCode));
        }
        return tokens;
    }
}
