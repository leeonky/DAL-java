package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Operator;

import java.util.Optional;

public interface OperatorMatcher {
    Optional<Operator> fetch(TokenParser tokenParser);

    default OperatorFactory or(OperatorFactory compiler) {
        return parser -> fetch(parser).orElseGet(() -> compiler.fetch(parser));
    }

    default OperatorFactory or(String message) {
        return parser -> fetch(parser).orElseThrow(() -> new SyntaxException(message, parser.getPosition()));
    }
}
