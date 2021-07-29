package com.github.leeonky.dal.token;

interface TokenCandidateFactory {

    TokenCandidate createTokenCandidate(SourceCode sourceCode);

    boolean isBegin(SourceCode sourceCode, Token lastToken);
}
