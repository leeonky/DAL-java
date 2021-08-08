package com.github.leeonky.dal.token;

import com.github.leeonky.dal.parser.ParsingContext;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import static com.github.leeonky.dal.DALCompiler.IS;
import static com.github.leeonky.dal.DALCompiler.WHICH;
import static com.github.leeonky.dal.token.TokenFactory.*;
import static java.util.Arrays.asList;

public class Scanner {
    //TODO to be removed use TOKEN_DELIMITER
    public static final Set<Character> CHAR_SPLIT = new HashSet<>(asList('(', ')', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!', '[', ']', ':'));
    public static final Set<Character> TOKEN_DELIMITER = new HashSet<>(asList('(', ')', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!', '[', ']', ':', ' ', '\t', '\n'));
    public static final Set<Character> OPERATOR_CHAR = new HashSet<>(asList('-', '!', '=', '>', '<', '+', '*', '/', ':', '&', '|'));
    public static final Set<Character> DIGITAL_CHAR = new HashSet<>(asList('1', '2', '3', '4', '5', '6', '7', '8', '9', '0'));
    public static final Set<String> KEYWORD_SETS = new HashSet<>(asList(IS, WHICH));
    public static final char OPT_MATCHES = ':';
    public static final String OPT_MATCHES_STRING = ":";

    //TODO to be replaced
    private final List<TokenCandidateFactory> tokenCandidateFactories = asList(
            AccessElementTokenCandidateFactory.INSTANCE
    );

    private final List<TokenFactory> tokenFactories = asList(
            createBeanPropertyTokenFactory(),
            createNumberTokenFactory(),
            createSingleQuotedStringTokenFactory(),
            createDoubleQuotedStringTokenFactory(),
            createRegexTokenFactory(),
            createOperatorTokenFactory(),
            createBeginBracketTokenFactory(),
            createEndBracketTokenFactory()
    );

    public TokenStream scan(SourceCode sourceCode) {
        TokenStream tokenStream = new TokenStream();
        ParsingContext context = new ParsingContext(sourceCode, null);
        while (sourceCode.hasContent()) {
            int begin = sourceCode.getPosition();
            Token token = tokenFactories.stream()
                    .map(tokenFactory -> tokenFactory.fetchToken(context))
                    .filter(Objects::nonNull).findFirst().orElseGet(() ->
                            takeTokenCandidate(sourceCode).fetchToken(sourceCode));
            //TODO move inside token create
            token.setPositionBegin(begin);
            token.setPositionEnd(sourceCode.getPosition());
            tokenStream.appendToken(token);
            context.last = token;
        }
        return tokenStream;
    }

    private TokenCandidate takeTokenCandidate(SourceCode sourceCode) {
        return tokenCandidateFactories.stream()
                .filter(f -> f.isBegin(sourceCode))
                .map(f -> f.createTokenCandidate(sourceCode))
                .findFirst().orElseGet(() -> new WordTokenCandidate(sourceCode));
    }
}
