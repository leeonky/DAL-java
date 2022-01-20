package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.Operator;

import java.util.Optional;

public interface OperatorMatcher<N extends Node<N>> {
    Optional<Operator<N>> fetch(TokenParser<N> tokenParser);

    default OperatorFactory<N> or(OperatorFactory<N> compiler) {
        return tokenParser -> fetch(tokenParser).orElseGet(() -> compiler.fetch(tokenParser));
    }

    default OperatorFactory<N> or(String message) {
        return tokenParser -> fetch(tokenParser).orElseThrow(() -> tokenParser.getSourceCode().syntaxError(message, 0));
    }
}
