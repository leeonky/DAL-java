package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.function.Function;

public interface TokenFactory {
    Token fetch(TokenParser tokenParser);

    default NodeFactory map(Function<Token, Node> mapper) {
        return parser -> mapper.apply(fetch(parser));
    }
}
