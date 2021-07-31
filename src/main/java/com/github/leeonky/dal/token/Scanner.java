package com.github.leeonky.dal.token;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import static com.github.leeonky.dal.DALCompiler.IS;
import static com.github.leeonky.dal.DALCompiler.WHICH;
import static java.util.Arrays.asList;

public class Scanner {
    public static final Set<Character> CHAR_SPLIT = new HashSet<>(asList('(', ')', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!'));
    public static final List<String> MULTI_CHAR_OPTS = asList(">=", "<=", "&&", "||", "!=");
    public static final Set<String> KEYWORD_SETS = new HashSet<>(asList(IS, WHICH));

    private List<TokenCandidateFactory> tokenCandidateFactories = asList(
            NumberTokenCandidateFactory.INSTANCE,
            PropertyTokenCandidateFactory.INSTANCE,
            AccessElementTokenCandidateFactory.INSTANCE,
            OperatorTokenCandidateFactory.INSTANCE,
            BeginBracketTokenCandidateFactory.INSTANCE,
            EndBracketTokenCandidateFactory.INSTANCE
    );

    private List<TokenFactory> tokenFactories = asList(
            new SingleQuotationStringTokenFactory(),
            new DoubleQuotationStringTokenFactory(),
            new RegexTokenFactory()
    );

    private Token lastToken;

    public TokenStream scan(SourceCode sourceCode) {
        TokenStream tokenStream = new TokenStream();
        lastToken = null;
        while (sourceCode.hasContent()) {
            int begin = sourceCode.getPosition();
            Token token = tokenFactories.stream().map(tokenFactory -> tokenFactory.fetchToken(sourceCode, lastToken))
                    .filter(Objects::nonNull).findFirst().orElseGet(() ->
                            takeTokenCandidate(sourceCode, lastToken).fetchToken(sourceCode));
            token.setPositionBegin(begin);
            token.setPositionEnd(sourceCode.getPosition());
            tokenStream.appendToken(token);
            lastToken = token;
        }
        return tokenStream;
    }

    private TokenCandidate takeTokenCandidate(SourceCode sourceCode, Token lastToken) {
        return tokenCandidateFactories.stream()
                .filter(f -> f.isBegin(sourceCode, lastToken))
                .map(f -> f.createTokenCandidate(sourceCode))
                .findFirst().orElseGet(() -> new WordTokenCandidate(sourceCode));
    }
}
