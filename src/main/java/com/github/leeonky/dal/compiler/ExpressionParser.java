package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.ExpressionIsSchema;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.SchemaNode;
import com.github.leeonky.dal.token.Token;

import java.util.function.BiFunction;

public class ExpressionParser implements BiFunction<NodeParser, Node, Node> {
    public static final ExpressionParser INSTANCE = new ExpressionParser();
    private final NodeFactory singleEvaluableNodeFactory = NodeFactory.createSingleEvaluableNodeFactory();
    private final OperatorExpressionFactory operatorExpressionFactory = new OperatorExpressionFactory();
    private final SchemaExpressionFactory schemaExpressionFactory = new SchemaExpressionFactory();

    @Override
    public Node apply(NodeParser nodeParser, Node first) {
        if (nodeParser.tokenStream.hasTokens()) {
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
                return new Expression(first,
                        nodeParser.tokenStream.pop().toOperator(false),
                        singleEvaluableNodeFactory.fetchNode(nodeParser)).adjustOperatorOrder();
            }
            return null;
        }
    }

    class SchemaExpressionFactory implements BiFunction<NodeParser, Node, Node> {

        @Override
        public Node apply(NodeParser nodeParser, Node node) {
            if (nodeParser.tokenStream.isCurrentKeywordAndTake(Constants.KeyWords.IS)) {
                ExpressionIsSchema expressionIsSchema = new ExpressionIsSchema(node, parseSchema(nodeParser));
                nodeParser.tokenStream.getSchemaOperators().forEach(opt ->
                        expressionIsSchema.appendSchema(opt, parseSchema(nodeParser)));
                return expressionIsSchema;
            }
            return null;
        }

        private SchemaNode parseSchema(NodeParser nodeParser) {
            if (nodeParser.tokenStream.hasTokens() && nodeParser.tokenStream.currentType() == Token.Type.WORD) {
                Token token = nodeParser.tokenStream.pop();
                return (SchemaNode) new SchemaNode((String) token.getValue()).setPositionBegin(token.getPositionBegin());
            }
            throw new SyntaxException(nodeParser.tokenStream.getPosition(), "operand of `is` must be schema type");
        }
    }
}
