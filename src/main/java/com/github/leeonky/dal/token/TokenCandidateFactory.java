package com.github.leeonky.dal.token;

import java.util.List;

import static java.util.Arrays.asList;

interface TokenCandidateFactory {

    List<TokenCandidateFactory> FACTORIES = asList(
            new NumberTokenCandidateFactory(),
            new PropertyTokenCandidateFactory(),
            new ConstIndexTokenCandidateFactory(),
            new OperatorTokenCandidateFactory(),
            new BeginBracketTokenCandidateFactory(),
            new EndBracketTokenCandidateFactory(),
            new SingleQuotationTokenCandidateFactory()
    );

    TokenCandidate createTokenCandidate(SourceCode sourceCode);

    boolean isBegin(SourceCode sourceCode);

}
