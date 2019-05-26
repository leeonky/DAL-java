package com.github.leeonky.dal.token;

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
            SingleQuotationStringTokenCandidateFactory.INSTANCE,
            DoubleQuotationStringTokenCandidateFactory.INSTANCE
    );

    public TokenStream scan(SourceCode sourceCode) {
        TokenStream tokenStream = new TokenStream();
        while (sourceCode.hasContent())
            tokenStream.appendToken(takeTokenCandidate(sourceCode).getToken(sourceCode));
        return tokenStream;
    }

    private TokenCandidate takeTokenCandidate(SourceCode sourceCode) {
        return factories.stream()
                .filter(f -> f.isBegin(sourceCode))
                .map(f -> f.createTokenCandidate(sourceCode))
                .findFirst().orElseGet(() -> new WordTokenCandidate(sourceCode));
    }
}
