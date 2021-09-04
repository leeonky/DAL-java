package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;

import java.util.LinkedList;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.ast.PropertyNode.Type.DOT;
import static com.github.leeonky.dal.token.Token.Type.*;
import static java.util.Optional.ofNullable;

public class NodeParser {
    private final LinkedList<Token> operators = new LinkedList<>();
    private final TokenStream tokenStream;
    private final ExpressionFactory expressionFactory = ((ExpressionFactory) NodeParser::compileOperatorExpression)
            .combine(NodeParser::compileSchemaWhichExpression);
    private int parenthesisCount = 0;

    public NodeParser(TokenStream tokenStream) {
        this.tokenStream = tokenStream;
    }

    public Node compileParentheses() {
        return tokenStream.parseBetween(OPENING_PARENTHESIS, CLOSING_PARENTHESIS, ')', () -> {
            try {
                parenthesisCount++;
                return new ParenthesesNode(NodeFactory.EXPRESSION.fetchNode(this));
            } finally {
                parenthesisCount--;
            }
        });
    }

    public Node compileBracketProperty(Node instance) {
        return tokenStream.parseBetween(OPENING_BRACKET, CLOSING_BRACKET, ']',
                () -> new PropertyNode(instance, tokenStream.popTokenForPropertyOrIndex(), BRACKET));
    }

    public Node compileIdentifierProperty() {
        return tokenStream.popByType(IDENTIFIER).map(Token::toIdentifierNode).orElse(null);
    }

    public Node compileExpression(Node previous) {
        return recursiveCompile(previous, expressionFactory, this::compileExpression);
    }

    private Node recursiveCompile(Node input, ExpressionFactory expressionFactory, Function<Node, Node> method) {
        if (tokenStream.hasTokens()) {
            processUnexpectedClosingParentheses();
            Node next = expressionFactory.fetchExpression(this, input);
            if (next != null)
                return method.apply(next);
        }
        return input;
    }

    private void processUnexpectedClosingParentheses() {
        if (tokenStream.isType(CLOSING_PARENTHESIS))
            if (parenthesisCount == 0)
                throw new SyntaxException(tokenStream.getPosition(), "missed '('");
    }

    public Node compileList() {
        //TODO should contains expression => a: 100+10
        return tokenStream.parseBetween(OPENING_BRACKET, CLOSING_BRACKET, ']', () -> {
            ListNode listNode = new ListNode();
            Token operatorToken = operators.isEmpty() ? Token.operatorToken(":") : operators.getFirst();
            int index = 0;
            //TODO refactor
            while (tokenStream.hasTokens() && !tokenStream.isType(CLOSING_BRACKET)) {
                //TODO use JudgementExpression type
                listNode.addJudgements(new Expression(
                        //TODO access element
                        //TODO not DOT mode
                        new PropertyNode(InputNode.INSTANCE, index++, DOT),
                        //TODO hardcode
                        operatorToken.toBinaryOperator(),
                        //TODO expression not finished
                        NodeFactory.RIGHT_OPERAND.fetchNode(this)));
            }
            return listNode;
        });
    }

    public Node compileObject() {
        //TODO should contains expression => a: 100+10
        return tokenStream.parseBetween(OPENING_BRACE, CLOSING_BRACE, '}', () -> {
            ObjectNode objectNode = new ObjectNode();
            //TODO refactor
            while (tokenStream.hasTokens() && !tokenStream.isType(CLOSING_BRACE)) {
                Node node = NodeFactory.PROPERTY.fetchNode(this);
                if (node != null)
                    //TODO use JudgementExpression type
                    objectNode.addJudgements(new Expression(
                            node,
                            tokenStream.pop().toBinaryOperator(),
                            //TODO expression not finished
                            NodeFactory.RIGHT_OPERAND.fetchNode(this)));
            }
            return objectNode;
        });
    }

    private Node giveDefault() {
        if (tokenStream.isFromBeginning())
            return InputNode.INSTANCE;
        throw new SyntaxException(tokenStream.getPosition(), "expect a value or expression");
    }

    private Node parsePropertyChain(Node instanceNode) {
        return recursiveCompile(instanceNode, NodeFactory.EXPLICIT_PROPERTY, this::parsePropertyChain);
    }

    public Node compileOperand() {
        if (!tokenStream.hasTokens())
            return giveDefault();

        return tokenStream.tryFetchUnaryOperator()
                .map(token -> (Node) new Expression(new ConstNode(null),
                        token.toUnaryOperator(), compileOperand()))
                .orElseGet(() -> parsePropertyChain(
                        ofNullable(NodeFactory.SINGLE_EVALUABLE.fetchNode(this))
                                .orElseGet(this::giveDefault)));
    }

    public Expression compileOperatorExpression(Node left) {
        return tokenStream.popByType(OPERATOR).map(operator -> withDefaultListJudgementOperator(operator, () ->
                tokenStream.shouldHasTokens("expression is not finished", () ->
                        new Expression(left, operator.toBinaryOperator(), compileRight(operator)).adjustOperatorOrder())))
                .orElse(null);
    }

    private Node compileRight(Token operator) {
        //TODO need UT
        return operator.judgement() ? NodeFactory.RIGHT_OPERAND.fetchNode(this) :
                NodeFactory.OPERAND.fetchNode(this);
    }


    private Expression withDefaultListJudgementOperator(Token operatorToken, Supplier<Expression> expressionSupplier) {
        operators.push(operatorToken);
        try {
            return expressionSupplier.get();
        } finally {
            operators.pop();
        }
    }

    private SchemaNode parseSchema() {
        return tokenStream.shouldHasTokens("schema expression not finished", () ->
                tokenStream.popByType(IDENTIFIER).map(Token::toSchemaNode).orElseThrow(() ->
                        new SyntaxException(tokenStream.getPosition(), "operand of `is` must be schema type")));
    }

    public Node compileSchemaWhichExpression(Node node) {
        return tokenStream.popKeyWord(Constants.KeyWords.IS)
                .map(is -> appendWhichClause(compileSchema(node, is))).orElse(null);
    }

    private Node appendWhichClause(SchemaExpression schemaExpression) {
        if (tokenStream.popKeyWord(Constants.KeyWords.WHICH).isPresent())
            return schemaExpression.which(NodeFactory.EXPRESSION.fetchNode(this));
        return schemaExpression;
    }

    private SchemaExpression compileSchema(Node node, Token is) {
        SchemaExpression schemaExpression = (SchemaExpression) new SchemaExpression(node, parseSchema())
                .setPositionBegin(is.getPositionBegin());
        while (tokenStream.isCurrentSchemaConnectorAndTake())
            schemaExpression.appendSchema(parseSchema());
        return schemaExpression;
    }

    public Node compileSingle(Token.Type type, Function<Token, Node> nodeFactory) {
        return tokenStream.popByType(type).map(nodeFactory).orElse(null);
    }
}
