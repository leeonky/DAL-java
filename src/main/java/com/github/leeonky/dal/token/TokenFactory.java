package com.github.leeonky.dal.token;

public interface TokenFactory {
    Token fetchToken(SourceCode sourceCode);
}
