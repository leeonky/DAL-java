package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.Node;

import static com.github.leeonky.dal.cucumber.ExpressionParser.BINARY_OPERATOR_EXPRESSION;
import static com.github.leeonky.dal.cucumber.ExpressionParser.SCHEMA_EXPRESSION;

public interface MandatoryExpressionParser {
    MandatoryExpressionParser EXPRESSION = BINARY_OPERATOR_EXPRESSION.combine(SCHEMA_EXPRESSION).defaultPrevious().recursive();

    Node fetch(SourceCode sourceCode, Node previous);

    default MandatoryExpressionParser recursive() {
        return (sourceCode, previous) -> fetch(sourceCode, fetch(sourceCode, previous));
    }
}
