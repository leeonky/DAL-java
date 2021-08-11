package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.Token;

public class ExpressionFactory {
    public static final ExpressionFactory INSTANCE = new ExpressionFactory();
    private final NodeFactory singleEvaluableNodeFactory = NodeFactory.createSingleEvaluableNodeFactory();
    private final OperatorExpressionFactory operatorExpressionFactory = new OperatorExpressionFactory();

    public Node parseExpression(NodeParser nodeParser, Node first) {
        if (nodeParser.tokenStream.hasTokens()) {
            Node expression = operatorExpressionFactory.parse(nodeParser, first);
            if (expression != null)
                return parseExpression(nodeParser, expression.setPositionBegin(first.getPositionBegin()));
        }
        return first;
    }

    class OperatorExpressionFactory {
        Node parse(NodeParser nodeParser, Node first) {
            if (nodeParser.tokenStream.currentType() == Token.Type.OPERATOR) {
                return new Expression(first,
                        nodeParser.tokenStream.pop().toOperator(false),
                        singleEvaluableNodeFactory.fetchNode(nodeParser)).adjustOperatorOrder();
            }
            return null;
        }
    }
}
