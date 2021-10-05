package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Node;

import java.util.Optional;
import java.util.function.Function;

public interface TokenParser {
    Optional<Token> fetch(SourceCode sourceCode);

    default TokenFactory or(String message) {
        return sourceCode -> fetch(sourceCode).orElseThrow(() -> new SyntaxException(message, sourceCode.getPosition()));
    }

    default NodeParser map(Function<Token, Node> mapper) {
        return sourceCode -> fetch(sourceCode).map(mapper);
    }
}
