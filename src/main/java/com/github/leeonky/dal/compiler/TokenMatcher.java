package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;
import java.util.function.Function;

public interface TokenMatcher {
    Optional<Token> fetch(TokenParser tokenParser);

    default TokenFactory or(String message) {
        return parser -> fetch(parser).orElseThrow(() -> new SyntaxException(message, parser.getPosition()));
    }

    default NodeMatcher map(Function<Token, Node> mapper) {
        return parser -> fetch(parser).map(mapper);
    }
}
