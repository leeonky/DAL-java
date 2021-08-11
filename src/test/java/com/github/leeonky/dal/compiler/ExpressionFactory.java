package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.Token;

public class ExpressionFactory {
    public static final ExpressionFactory INSTANCE = new ExpressionFactory();
    private final NodeFactory singleEvaluableNodeFactory = NodeFactory.createSingleEvaluableNodeFactory();

    public Node parseExpression(NodeParser nodeParser, Node first) {
        if (nodeParser.tokenStream.hasTokens()
                && nodeParser.tokenStream.currentType() == Token.Type.OPERATOR) {
            Expression expression = new Expression(first,
                    nodeParser.tokenStream.pop().toOperator(false),
                    singleEvaluableNodeFactory.fetchNode(nodeParser));
            return parseExpression(nodeParser, expression.setPositionBegin(first.getPositionBegin()));
        }
        return first;
    }
}
