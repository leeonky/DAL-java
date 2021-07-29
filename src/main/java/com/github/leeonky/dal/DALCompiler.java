package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.token.Scanner;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;

import java.util.Optional;

import static java.util.Optional.of;
import static java.util.Optional.ofNullable;

public class DALCompiler {
    public static final String WHICH = "which";
    public static final String IS = "is";
    public static final String NULL = "null";
    public static final String TRUE = "true";
    public static final String FALSE = "false";
    public static final String AND = "and";
    public static final String OR = "or";
    public static final String MATCHES = "matches";
    private Scanner scanner = new Scanner();

    public Node compile(SourceCode sourceCode) {
        return compileExpression(scanner.scan(sourceCode), null, true);
    }

    private Node compileExpression(TokenStream tokenStream, BracketNode bracketNode, boolean referenceInput) {
        Node node = compileOneNode(tokenStream, referenceInput)
                .orElseThrow(() -> new SyntaxException(tokenStream.getPosition(), "expect a value"));
        while (tokenStream.hasTokens() && !tokenStream.isCurrentEndBracketAndTakeThenFinishBracket(bracketNode)) {
            if (tokenStream.isCurrentKeywordAndTake(IS)) {
                node = new SchemaAssertionExpression(node, compileTypeNode(tokenStream).orElseThrow(() ->
                        new SyntaxException(tokenStream.getPosition(), "operand of `is` must be a type")),
                        compileWhichClause(tokenStream, bracketNode));
                for (SchemaAssertionExpression.Operator operator : tokenStream.getSchemaOperators()) {
                    ((SchemaAssertionExpression) node).appendSchema(operator, compileTypeNode(tokenStream)
                            .orElseThrow(() -> new SyntaxException(tokenStream.getPosition(), "Schema expression not finished")));
                }
            } else
                node = new Expression(node, tokenStream.pop().toOperator(false),
                        compileOneNode(tokenStream, false).orElseThrow(() ->
                                new SyntaxException(tokenStream.getPosition(), "expression not finished")))
                        .adjustOperatorOrder();
        }
        return node;
    }

    private Node compileWhichClause(TokenStream tokenStream, BracketNode bracketNode) {
        return (tokenStream.hasTokens() && tokenStream.isCurrentKeywordAndTake(WHICH)) ?
                compileExpression(tokenStream, bracketNode, false) : new ConstNode(true);
    }

    private Optional<Node> compileOneNode(TokenStream tokenStream, boolean isFirstNode) {
        if (tokenStream.hasTokens() && tokenStream.isSingleUnaryOperator(isFirstNode))
            return of(new Expression(new ConstNode(null), tokenStream.pop().toOperator(true),
                    compileOneNode(tokenStream, false)
                            .orElseThrow(() -> new SyntaxException(tokenStream.getPosition(), "expect a value"))));
        return ofNullable(compilePropertyOrIndexChain(tokenStream, compileOperand(tokenStream, isFirstNode)));
    }

    private Node compilePropertyOrIndexChain(TokenStream tokenStream, Node node) {
        while (tokenStream.hasTokens() && tokenStream.isCurrentSingleEvaluateNode()) {
            Token token = tokenStream.pop();
            switch (token.getType()) {
                case PROPERTY:
                    node = new PropertyNode(node, token.getProperties());
                    node.setPositionBegin(token.getPositionBegin());
                    break;
                case CONST_INDEX:
                    node = new Expression(node, new Operator.Index(), new ConstNode(token.getValue()));
                    node.setPositionBegin(token.getPositionBegin());
                    break;
            }
        }
        return node;
    }

    private Node compileOperand(TokenStream tokenStream, boolean isFirstNode) {
        Node node = null;
        if (tokenStream.hasTokens()) {
            if (tokenStream.currentType() == Token.Type.CONST_VALUE)
                node = new ConstNode(tokenStream.pop().getValue());
            else if (tokenStream.isCurrentSingleEvaluateNode())
                node = InputNode.INSTANCE;
            else if (tokenStream.isCurrentBeginBracket())
                node = compileBracket(tokenStream);
            else if (tokenStream.isCurrentBeginRegex())
                //TODO
                node = new RegexNode("1");
        }
        if (isFirstNode && node == null)
            node = InputNode.INSTANCE;
        return node;
    }

    private Optional<SchemaNode> compileTypeNode(TokenStream tokenStream) {
        SchemaNode node = null;
        if (tokenStream.hasTokens() && tokenStream.currentType() == Token.Type.WORD) {
            int position = tokenStream.getPosition();
            node = new SchemaNode(tokenStream.pop().getValue().toString());
            node.setPositionBegin(position);
        }
        return ofNullable(node);
    }

    private BracketNode compileBracket(TokenStream tokenStream) {
        Token bracketToken = tokenStream.pop();
        BracketNode bracketNode = new BracketNode();
        bracketNode.setNode(compileExpression(tokenStream, bracketNode, false));
        if (!bracketNode.isFinished())
            throw new SyntaxException(bracketToken.getPositionBegin(), "missed end bracket");
        return bracketNode;
    }
}
