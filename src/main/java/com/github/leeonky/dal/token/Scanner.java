package com.github.leeonky.dal.token;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static java.util.Arrays.asList;

public class Scanner {
    public static final Set<Character> CHAR_SPLIT = new HashSet<>(asList('(', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!'));
    static final List<String> MULTI_CHAR_OPTS = asList(">=", "<=", "&&", "||", "!=");

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
            tokens.add(takeTokenCandidate(sourceCode).getToken(sourceCode));
        return tokens;
    }

    private TokenCandidate takeTokenCandidate(SourceCode sourceCode) {
        return factories.stream()
                .filter(f -> f.isBegin(sourceCode))
                .map(f -> f.createTokenCandidate(sourceCode))
                .findFirst().orElseGet(() -> new WordTokenCandidate(sourceCode));
    }
}
