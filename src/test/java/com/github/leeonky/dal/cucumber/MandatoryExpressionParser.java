package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.Node;

public interface MandatoryExpressionParser {

    MandatoryExpressionParser EXPRESSION = (sourceCode, previous) ->
            ExpressionParser.BINARY_OPERATOR_EXPRESSION.fetch(sourceCode, previous).orElse(previous);

    Node fetch(SourceCode sourceCode, Node previous);
}
