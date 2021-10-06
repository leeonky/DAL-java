package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Operator;

import java.util.Optional;

public interface OperatorMatcher {
    Optional<Operator> fetch(SourceCode sourceCode);

    default OperatorFactory or(OperatorFactory compiler) {
        return sourceCode -> fetch(sourceCode).orElseGet(() -> compiler.fetch(sourceCode));
    }

    default OperatorFactory or(String message) {
        return sourceCode -> fetch(sourceCode).orElseThrow(() -> new SyntaxException(message, sourceCode.getPosition()));
    }
}
