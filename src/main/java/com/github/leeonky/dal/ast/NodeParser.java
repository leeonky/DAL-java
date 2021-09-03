package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;

import java.util.LinkedList;
import java.util.function.Function;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.ast.PropertyNode.Type.DOT;
import static com.github.leeonky.dal.token.Token.Type.*;
import static java.util.Optional.ofNullable;

public class NodeParser {
    private final LinkedList<Token> operators = new LinkedList<>();
    private final TokenStream tokenStream;
    private final ExpressionFactory expressionFactory = ((ExpressionFactory) NodeParser::compileOperatorExpression)
            .combine(NodeParser::compileSchemaExpression);
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
        if (tokenStream.hasTokens()) {
            processUnexpectedClosingParentheses();
            Node expression = expressionFactory.fetchExpression(this, previous);
            if (expression != null)
                return compileExpression(expression.setPositionBegin(previous.getPositionBegin()));
        }
        return previous;
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
            if (tokenStream.hasTokens()) {
                int index = 0;
                //TODO refactor
                while (tokenStream.hasTokens() && !tokenStream.isType(CLOSING_BRACKET)) {
                    //TODO use JudgementExpression type
                    listNode.addJudgements(new Expression(
                            //TODO access element
                            //TODO not DOT mode
                            new PropertyNode(InputNode.INSTANCE, index++, DOT),
                            //TODO hardcode
                            operatorToken.toOperator(false),
                            //TODO expression not finished
                            NodeFactory.RIGHT_OPERAND.fetchNode(this)));
                }
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
                            tokenStream.pop().toOperator(false),
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

    private Node parsePropertyChain(Node node) {
        if (tokenStream.hasTokens()) {
            Node next = NodeFactory.EXPLICIT_PROPERTY.fetchExpression(this, node);
            if (next != null)
                return parsePropertyChain(next);
        }
        return node;
    }

    public Node compileOperand() {
        if (!tokenStream.hasTokens())
            return giveDefault();

        return tokenStream.tryFetchUnaryOperator()
                .map(token -> (Node) new Expression(new ConstNode(null),
                        token.toOperator(true), compileOperand()))
                .orElseGet(() -> parsePropertyChain(
                        ofNullable(NodeFactory.SINGLE_EVALUABLE.fetchNode(this))
                                .orElseGet(this::giveDefault)));
    }

    public Expression compileOperatorExpression(Node first) {
        if (tokenStream.isType(OPERATOR)) {
            Token operatorToken = tokenStream.pop();
            operators.push(operatorToken);
            try {
                if (tokenStream.hasTokens())
                    return new Expression(first, operatorToken.toOperator(false),
                            //TODO need UT
                            operatorToken.judgement() ?
                                    NodeFactory.RIGHT_OPERAND.fetchNode(this) :
                                    NodeFactory.OPERAND.fetchNode(this)
                    ).adjustOperatorOrder();
                throw new SyntaxException(tokenStream.getPosition(), "expression is not finished");
            } finally {
                operators.pop();
            }
        }
        return null;
    }

    private SchemaNode parseSchema() {
        if (!tokenStream.hasTokens())
            throw new SyntaxException(tokenStream.getPosition(), "schema expression not finished");
        if (!tokenStream.isType(IDENTIFIER))
            throw new SyntaxException(tokenStream.getPosition(), "operand of `is` must be schema type");
        Token token = tokenStream.pop();
        return (SchemaNode) new SchemaNode((String) token.getValue()).setPositionBegin(token.getPositionBegin());
    }

    public Node compileSchemaExpression(Node node) {
        if (tokenStream.isCurrentKeywordAndTake(Constants.KeyWords.IS)) {
            SchemaExpression schemaExpression = new SchemaExpression(node, parseSchema());
            while (tokenStream.isCurrentSchemaConnectorAndTake())
                schemaExpression.appendSchema(parseSchema());
            if (tokenStream.isCurrentKeywordAndTake(Constants.KeyWords.WHICH))
                return schemaExpression.which(NodeFactory.EXPRESSION.fetchNode(this));
            return schemaExpression;
        }
        return null;
    }

    public Node compileSingle(Token.Type type, Function<Token, Node> nodeFactory) {
        return tokenStream.popByType(type).map(token ->
                nodeFactory.apply(token).setPositionBegin(token.getPositionBegin())).orElse(null);
    }
}
