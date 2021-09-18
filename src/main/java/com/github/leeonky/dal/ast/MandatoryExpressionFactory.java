package com.github.leeonky.dal.ast;

public interface MandatoryExpressionFactory {
    MandatoryExpressionFactory EXPRESSION = NodeParser::compileExpression;

    Node fetch(NodeParser nodeParser, Node previous);
}
