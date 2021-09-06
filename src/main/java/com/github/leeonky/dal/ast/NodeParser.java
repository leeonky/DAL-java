package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;

import java.util.LinkedList;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.dal.ast.NodeFactory.*;
import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.token.Token.Type.*;

public class NodeParser {
    private final LinkedList<Token> operators = new LinkedList<>();
    private final TokenStream tokenStream;
    private final ExpressionFactory expressionFactory = ((ExpressionFactory) NodeParser::compileOperatorExpression)
            .combine(NodeParser::compileSchemaWhichExpression);

    public NodeParser(TokenStream tokenStream) {
        this.tokenStream = tokenStream;
    }

    public Optional<Node> compileParentheses() {
        return tokenStream.parseBetween(OPENING_PARENTHESIS, CLOSING_PARENTHESIS, ')',
                () -> new ParenthesesNode(NodeFactory.EXPRESSION.fetch(this)));
    }

    public Optional<Node> compileBracketProperty(Node instance) {
        return tokenStream.parseBetween(OPENING_BRACKET, CLOSING_BRACKET, ']',
                () -> new PropertyNode(instance, tokenStream.popTokenForPropertyOrIndex(), BRACKET));
    }

    public Optional<Node> compileIdentifierProperty() {
        return tokenStream.popByType(IDENTIFIER).map(Token::toIdentifierNode);
    }

    public Node compileExpression(Node previous) {
        return recursiveCompile(previous, expressionFactory, this::compileExpression);
    }

    public Node compileCalculateExpression(Node previous) {
        return recursiveCompile(previous, NodeParser::compileCalculateOperatorExpression,
                this::compileCalculateExpression);
    }

    private Node recursiveCompile(Node input, ExpressionFactory expressionFactory, Function<Node, Node> method) {
        return tokenStream.fetchNode(() -> {
            tokenStream.checkingParenthesis();
            return expressionFactory.tryFetch(this, input).map(method).orElse(input);
        }).orElse(input);
    }

    public Optional<Node> compileList() {
        //TODO should contains expression => a: 100+10
        return tokenStream.parseBetween(OPENING_BRACKET, CLOSING_BRACKET, ']', () ->
                new ListNode(tokenStream.fetchElements(CLOSING_BRACKET, this::compileElementNode)));
    }

    private Expression compileElementNode(Integer index) {
        return new Expression(new PropertyNode(InputNode.INSTANCE, index, BRACKET),
                tokenStream.popJudgementOperator().orElseGet(this::defaultListOperator).toBinaryOperator(),
                JUDGEMENT_OPERAND.fetch(this));
    }

    private Token defaultListOperator() {
        return operators.isEmpty() ? Token.operatorToken(":") : operators.getFirst();
    }

    public Optional<Node> compileObject() {
        //TODO should contains expression => a: 100+10
        return tokenStream.parseBetween(OPENING_BRACE, CLOSING_BRACE, '}', () ->
                new ObjectNode(tokenStream.fetchElements(CLOSING_BRACE, index -> compilePropertyJudgementExpression())));
    }

    private Expression compilePropertyJudgementExpression() {
        return new Expression(fetch(NodeFactory.PROPERTY.tryFetch(this), "expect a object property"),
                fetch(tokenStream.popJudgementOperator(), "expect operator `:` or `=`").toBinaryOperator(),
                RIGHT_OPERAND.fetch(this));
    }

    private <T> T fetch(Optional<T> node, String errorMessage) {
        return node.orElseThrow(() -> new SyntaxException(tokenStream.getPosition(), errorMessage));
    }

    private Node giveDefault() {
        if (tokenStream.isFromBeginning())
            return InputNode.INSTANCE;
        throw new SyntaxException(tokenStream.getPosition(), "expect a value or expression");
    }

    public Node compileOperand() {
        return tokenStream.fetchNode(() -> tokenStream.tryFetchUnaryOperator()
                .map(token -> (Node) new Expression(new ConstNode(null), token.toUnaryOperator(), compileOperand()))
                .orElseGet(() -> parsePropertyChain(NodeFactory.SINGLE_EVALUABLE.tryFetch(this)
                        .orElseGet(this::giveDefault))))
                .orElseGet(this::giveDefault);
    }

    private Node parsePropertyChain(Node instanceNode) {
        return recursiveCompile(instanceNode, ExpressionFactory.EXPLICIT_PROPERTY,
                this::parsePropertyChain);
    }

    private Optional<Node> compileOperatorExpression(Node left) {
        return tokenStream.popByType(OPERATOR).map(operator -> withDefaultListJudgementOperator(operator, () ->
                fetch(tokenStream.fetchNode(() -> new Expression(left, operator.toBinaryOperator(),
                        compileRight(operator)).adjustOperatorOrder()), "expression is not finished")));
    }

    public Optional<Node> compileCalculateOperatorExpression(Node left) {
        return tokenStream.popCalculatorOperator()
                .map(operator -> fetch(tokenStream.fetchNode(() -> new Expression(left, operator.toBinaryOperator(),
                        OPERAND.fetch(this)).adjustOperatorOrder()), "expression is not finished"));
    }

    private Node compileRight(Token operator) {
        return operator.judgement() ? RIGHT_OPERAND.fetch(this)
                : NodeFactory.OPERAND.fetch(this);
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
        return fetch(tokenStream.fetchNode(() -> fetch(tokenStream.popByType(IDENTIFIER).map(Token::toSchemaNode),
                "operand of `is` must be schema type")), "schema expression not finished");
    }

    public Optional<Node> compileSchemaWhichExpression(Node node) {
        return tokenStream.popKeyWord(Constants.KeyWords.IS)
                .map(is -> appendWhichClause(compileSchema(node, is)));
    }

    private Node appendWhichClause(SchemaExpression schemaExpression) {
        if (tokenStream.popKeyWord(Constants.KeyWords.WHICH).isPresent())
            return schemaExpression.which(NodeFactory.EXPRESSION.fetch(this));
        return schemaExpression;
    }

    private SchemaExpression compileSchema(Node node, Token is) {
        SchemaExpression schemaExpression = (SchemaExpression) new SchemaExpression(node, parseSchema())
                .setPositionBegin(is.getPositionBegin());
        while (tokenStream.isCurrentSchemaConnectorAndTake())
            schemaExpression.appendSchema(parseSchema());
        return schemaExpression;
    }

    public Optional<Node> compileSingle(Token.Type type, Function<Token, Node> nodeFactory) {
        return tokenStream.popByType(type).map(t -> nodeFactory.apply(t).setPositionBegin(t.getPositionBegin()));
    }
}
