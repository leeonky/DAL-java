package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.function.Function;

public interface TokenFactory {
    Token fetch(SourceCode sourceCode);

    default NodeFactory map(Function<Token, Node> mapper) {
        return parser -> {
            Token token = fetch(parser.getSourceCode());
            return mapper.apply(token).setPositionBegin(token.getPosition());
        };
    }
}