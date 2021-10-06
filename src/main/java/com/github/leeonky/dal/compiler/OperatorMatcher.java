package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Operator;

import java.util.Optional;

public interface OperatorMatcher {
    Optional<Operator> fetch(TokenParser tokenParser);

    default OperatorFactory or(OperatorFactory compiler) {
        return tokenParser -> fetch(tokenParser).orElseGet(() -> compiler.fetch(tokenParser));
    }

    default OperatorFactory or(String message) {
        return tokenParser -> fetch(tokenParser).orElseThrow(() -> tokenParser.getSourceCode().syntaxError(message, 0));
    }
}
