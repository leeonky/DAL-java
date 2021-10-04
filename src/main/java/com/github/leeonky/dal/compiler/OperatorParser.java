package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Operator;

import java.util.Optional;

public interface OperatorParser {
    Optional<Operator> fetch(SourceCode sourceCode);

    default OperatorCompiler or(OperatorCompiler compiler) {
        return sourceCode -> fetch(sourceCode).orElseGet(() -> compiler.fetch(sourceCode));
    }

    default OperatorCompiler or(String message) {
        return sourceCode -> fetch(sourceCode).orElseThrow(() -> new SyntaxException(message, sourceCode.getPosition()));
    }
}
