package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.function.Function;

public interface TokenFactory {
    Token fetch(SourceCode sourceCode);

    default NodeCompiler map(Function<Token, Node> mapper) {
        return sourceCode -> mapper.apply(fetch(sourceCode));
    }
}
