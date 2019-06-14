package com.github.leeonky.dal.token;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static com.github.leeonky.dal.DALCompiler.IS;
import static com.github.leeonky.dal.DALCompiler.WHICH;
import static java.util.Arrays.asList;

public class Scanner {
    public static final Set<Character> CHAR_SPLIT = new HashSet<>(asList('(', ')', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!'));
    public static final List<String> MULTI_CHAR_OPTS = asList(">=", "<=", "&&", "||", "!=");
    public static final Set<String> KEYWORD_SETS = new HashSet<>(asList(IS, WHICH));

    private List<TokenCandidateFactory> factories = asList(
            NumberTokenCandidateFactory.INSTANCE,
            PropertyTokenCandidateFactory.INSTANCE,
            AccessElementTokenCandidateFactory.INSTANCE,
            OperatorTokenCandidateFactory.INSTANCE,
            BeginBracketTokenCandidateFactory.INSTANCE,
            EndBracketTokenCandidateFactory.INSTANCE,
            SingleQuotationStringTokenCandidateFactory.INSTANCE,
            DoubleQuotationStringTokenCandidateFactory.INSTANCE
    );

    public TokenStream scan(SourceCode sourceCode) {
        TokenStream tokenStream = new TokenStream();
        while (sourceCode.hasContent()) {
            int begin = sourceCode.getPosition();
            Token token = takeTokenCandidate(sourceCode).getToken(sourceCode);
            token.setPositionBegin(begin);
            token.setPositionEnd(sourceCode.getPosition());
            tokenStream.appendToken(token);
        }
        return tokenStream;
    }

    private TokenCandidate takeTokenCandidate(SourceCode sourceCode) {
        return factories.stream()
                .filter(f -> f.isBegin(sourceCode))
                .map(f -> f.createTokenCandidate(sourceCode))
                .findFirst().orElseGet(() -> new WordTokenCandidate(sourceCode));
    }
}
