package com.github.leeonky.dal.token;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;

public class Scanner {
    private List<TokenCandidateFactory> factories = asList(
            NumberTokenCandidateFactory.INSTANCE,
            PropertyTokenCandidateFactory.INSTANCE,
            ConstIndexTokenCandidateFactory.INSTANCE,
            OperatorTokenCandidateFactory.INSTANCE,
            BeginBracketTokenCandidateFactory.INSTANCE,
            EndBracketTokenCandidateFactory.INSTANCE,
            SingleQuotationTokenCandidateFactory.INSTANCE
    );

    public List<Token> scan(SourceCode sourceCode) {
        List<Token> tokens = new ArrayList<>();
        while (sourceCode.hasContent())
            tokens.add(getOneToken(sourceCode));
        return tokens;
    }

    private Token getOneToken(SourceCode sourceCode) {
        return takeTokenCandidate(sourceCode).getToken(sourceCode);
    }

    private TokenCandidate takeTokenCandidate(SourceCode sourceCode) {
        return factories.stream()
                .filter(f -> f.isBegin(sourceCode))
                .map(f -> f.createTokenCandidate(sourceCode))
                .findFirst().orElseGet(() -> new WordTokenCandidate(sourceCode));
    }
}
