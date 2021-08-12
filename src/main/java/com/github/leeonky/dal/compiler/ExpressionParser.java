package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.SchemaNode;
import com.github.leeonky.dal.ast.TypeExpression;
import com.github.leeonky.dal.token.Token;

import java.util.function.BiFunction;

public class ExpressionParser implements BiFunction<NodeParser, Node, Node> {
    public static final ExpressionParser INSTANCE = new ExpressionParser();
    private final NodeFactory singleEvaluableNodeFactory = NodeFactory.createSingleEvaluableNodeFactory();
    private final OperatorExpressionFactory operatorExpressionFactory = new OperatorExpressionFactory();
    private final SchemaExpressionFactory schemaExpressionFactory = new SchemaExpressionFactory();
    private final NodeFactory regexNodeFactory = NodeFactory.createRegexNodeFactory();
    private final NodeFactory expressionNodeFactory = NodeFactory.createExpressionNodeFactory();

    @Override
    public Node apply(NodeParser nodeParser, Node first) {
        if (nodeParser.tokenStream.hasTokens()) {
            //TODO refactor
            Node expression = operatorExpressionFactory.apply(nodeParser, first);
            if (expression == null)
                expression = schemaExpressionFactory.apply(nodeParser, first);
            if (expression != null)
                return apply(nodeParser, expression.setPositionBegin(first.getPositionBegin()));
        }
        return first;
    }

    class OperatorExpressionFactory implements BiFunction<NodeParser, Node, Node> {

        @Override
        public Node apply(NodeParser nodeParser, Node first) {
            if (nodeParser.tokenStream.currentType() == Token.Type.OPERATOR) {
                Token operatorToken = nodeParser.tokenStream.pop();
                return new Expression(first, operatorToken.toOperator(false),
                        getSecondOperand(operatorToken, nodeParser)).adjustOperatorOrder();
            }
            return null;
        }

        private Node getSecondOperand(Token operatorToken, NodeParser nodeParser) {
            if (operatorToken.judgement()) {
                Node node = regexNodeFactory.fetchNode(nodeParser);
                if (node != null)
                    return node;
            }
            return singleEvaluableNodeFactory.fetchNode(nodeParser);
        }
    }

    class SchemaExpressionFactory implements BiFunction<NodeParser, Node, Node> {

        @Override
        public Node apply(NodeParser nodeParser, Node node) {
            if (nodeParser.tokenStream.isCurrentKeywordAndTake(Constants.KeyWords.IS)) {
                TypeExpression typeExpression = new TypeExpression(node, parseSchema(nodeParser));
                nodeParser.tokenStream.getSchemaOperators().forEach(opt ->
                        typeExpression.appendSchema(opt, parseSchema(nodeParser)));
                if (nodeParser.tokenStream.isCurrentKeywordAndTake(Constants.KeyWords.WHICH))
                    return typeExpression.which(expressionNodeFactory.fetchNode(nodeParser));
                return typeExpression;
            }
            return null;
        }

        private SchemaNode parseSchema(NodeParser nodeParser) {
            if (nodeParser.tokenStream.hasTokens()) {
                if (nodeParser.tokenStream.currentType() == Token.Type.WORD) {
                    Token token = nodeParser.tokenStream.pop();
                    return (SchemaNode) new SchemaNode((String) token.getValue()).setPositionBegin(token.getPositionBegin());
                } else
                    throw new SyntaxException(nodeParser.tokenStream.getPosition(), "operand of `is` must be schema type");
            }
            throw new SyntaxException(nodeParser.tokenStream.getPosition(), "Schema expression not finished");
        }
    }
}
