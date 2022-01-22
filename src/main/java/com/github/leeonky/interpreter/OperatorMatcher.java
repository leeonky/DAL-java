package com.github.leeonky.interpreter;

import com.github.leeonky.dal.ast.DALOperator;

import java.util.Optional;

public interface OperatorMatcher<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E>> {
    Optional<DALOperator> fetch(TokenParser<E, N, C> tokenParser);

    default OperatorFactory<C, N, E> or(OperatorFactory<C, N, E> compiler) {
        return tokenParser -> fetch(tokenParser).orElseGet(() -> compiler.fetch(tokenParser));
    }

    default OperatorFactory<C, N, E> or(String message) {
        return tokenParser -> fetch(tokenParser).orElseThrow(() -> tokenParser.getSourceCode().syntaxError(message, 0));
    }
}
