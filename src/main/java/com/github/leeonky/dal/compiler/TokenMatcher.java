package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;
import java.util.function.Function;

public interface TokenMatcher {
    Optional<Token> fetch(SourceCode sourceCode);

    default TokenFactory or(String message) {
        return sourceCode -> fetch(sourceCode).orElseThrow(() -> sourceCode.syntaxError(message, 0));
    }

    default NodeMatcher map(Function<Token, Node> mapper) {
        return parser -> fetch(parser.getSourceCode()).map(token ->
                mapper.apply(token).setPositionBegin(token.getPosition()));
    }
}
