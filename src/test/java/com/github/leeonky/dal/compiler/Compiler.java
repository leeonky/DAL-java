package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.token.TokenStream;

public class Compiler {
    //TODO to be private
    final TokenStream tokenStream;

    public Compiler(TokenStream tokenStream) {
        this.tokenStream = tokenStream;
    }
}
