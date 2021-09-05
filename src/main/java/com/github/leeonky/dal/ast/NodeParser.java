package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;

import java.util.LinkedList;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.dal.ast.NodeFactory.SINGLE_EVALUABLE;
import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
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
        return tokenStream.parseBetween(OPENING_PARENTHESIS, CLOSING_PARENTHESIS, ')',
                this::compileParenthesesNode);
    }

    private ParenthesesNode compileParenthesesNode() {
        try {
            parenthesisCount++;
            return new ParenthesesNode(NodeFactory.EXPRESSION.fetchNode(this));
        } finally {
            parenthesisCount--;
        }
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
        return tokenStream.fetchNode(() -> {
            processUnexpectedClosingParentheses();
            return expressionFactory.fetchExpressionOptional(this, input).map(method).orElse(input);
        }).orElse(input);
    }

    private void processUnexpectedClosingParentheses() {
        if (tokenStream.isType(CLOSING_PARENTHESIS) && parenthesisCount == 0)
            throw new SyntaxException(tokenStream.getPosition(), "missed '('");
    }

    public Node compileList() {
        //TODO should contains expression => a: 100+10
        return tokenStream.parseBetween(OPENING_BRACKET, CLOSING_BRACKET, ']', () ->
                new ListNode(tokenStream.fetchElements(CLOSING_BRACKET, index -> {
                    //TODO use JudgementExpression type
                    return new Expression(
                            new PropertyNode(InputNode.INSTANCE, index, BRACKET),
                            //TODO support element judgement
                            defaultListOperator().toBinaryOperator(),
                            //TODO expression not finished
                            NodeFactory.RIGHT_OPERAND.fetchNode(this));
                })));
    }

    private Token defaultListOperator() {
        return operators.isEmpty() ? Token.operatorToken(":") : operators.getFirst();
    }

    public Node compileObject() {
        //TODO should contains expression => a: 100+10
        return tokenStream.parseBetween(OPENING_BRACE, CLOSING_BRACE, '}', () ->
                new ObjectNode(tokenStream.fetchElements(CLOSING_BRACE, index -> {
                    Node node = NodeFactory.PROPERTY.fetchNode(this);
                    //TODO null node / node is not start with property
                    //TODO use JudgementExpression type
                    return new Expression(
                            node,
                            tokenStream.pop().toBinaryOperator(),
                            //TODO expression not finished
                            NodeFactory.RIGHT_OPERAND.fetchNode(this));
                })));
    }

    private Node giveDefault() {
        if (tokenStream.isFromBeginning())
            return InputNode.INSTANCE;
        throw new SyntaxException(tokenStream.getPosition(), "expect a value or expression");
    }

    public Node compileOperand() {
        return tokenStream.fetchNode(() -> tokenStream.tryFetchUnaryOperator()
                .map(token -> (Node) new Expression(new ConstNode(null), token.toUnaryOperator(), compileOperand()))
                .orElseGet(() -> parsePropertyChain(ofNullable(SINGLE_EVALUABLE.fetchNode(this))
                        .orElseGet(this::giveDefault))))
                .orElseGet(this::giveDefault);
    }

    private Node parsePropertyChain(Node instanceNode) {
        return recursiveCompile(instanceNode, NodeFactory.EXPLICIT_PROPERTY, this::parsePropertyChain);
    }

    public Expression compileOperatorExpression(Node left) {
        return tokenStream.popByType(OPERATOR).map(operator -> withDefaultListJudgementOperator(operator, () ->
                tokenStream.fetchNode(() ->
                        new Expression(left, operator.toBinaryOperator(), compileRight(operator)).adjustOperatorOrder())
                        .orElseThrow(() -> new SyntaxException(tokenStream.getPosition(), "expression is not finished"))
        )).orElse(null);
    }

    private Node compileRight(Token operator) {
        return operator.judgement() ? NodeFactory.RIGHT_OPERAND.fetchNode(this)
                : NodeFactory.OPERAND.fetchNode(this);
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
        return tokenStream.fetchNode(() ->
                tokenStream.popByType(IDENTIFIER).map(Token::toSchemaNode).orElseThrow(() ->
                        new SyntaxException(tokenStream.getPosition(), "operand of `is` must be schema type")))
                .orElseThrow(() -> new SyntaxException(tokenStream.getPosition(), "schema expression not finished"));
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
        return tokenStream.popByType(type).map(t ->
                nodeFactory.apply(t).setPositionBegin(t.getPositionBegin())).orElse(null);
    }
}
