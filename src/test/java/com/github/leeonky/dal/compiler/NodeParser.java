package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.token.TokenStream;

public class NodeParser {
    //TODO to be private
    final TokenStream tokenStream;

    public NodeParser(TokenStream tokenStream) {
        this.tokenStream = tokenStream;
    }
}
