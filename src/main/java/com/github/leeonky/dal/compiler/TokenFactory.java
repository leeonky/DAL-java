package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.interpreter.SourceCode;

import java.util.function.Function;

public interface TokenFactory {
    Token fetch(SourceCode sourceCode);

    default <N extends Node<N>> NodeFactory<N> map(Function<Token, N> mapper) {
        return parser -> {
            Token token = fetch(parser.getSourceCode());
            return mapper.apply(token).setPositionBegin(token.getPosition());
        };
    }
}
