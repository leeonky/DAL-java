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
    private Scanner scanner = new Scanner();

    public static String getClassName(Object obj) {
        return obj == null ? null : obj.getClass().getName();
    }

    public Node compile(SourceCode sourceCode) {
        return compileExpression(scanner.scan(sourceCode), null, true);
    }

    private Node compileExpression(TokenStream tokenStream, BracketNode bracketNode, boolean referenceInput) {
        Node node = compileValueNode(tokenStream, referenceInput).get();
        while (tokenStream.hasTokens() && !tokenStream.isCurrentEndBracketAndTakeThenFinishBracket(bracketNode))
            node = new Expression(node, tokenStream.pop().toOperator(false),
                    compileValueNode(tokenStream, false)
                            .orElseGet(() -> compileTypeNode(tokenStream)
                                    .orElseThrow(() -> new SyntaxException(tokenStream.getPosition(), "expression not finished"))))
                    .adjustOperatorOrder();
        return node;
    }

    private Optional<Node> compileValueNode(TokenStream tokenStream, boolean isFirstNode) {
        Node node = null;
        if (tokenStream.hasTokens()) {
            if (tokenStream.currentType() == Token.Type.CONST_VALUE)
                node = new ConstNode(tokenStream.pop().getValue());
            else if (tokenStream.isCurrentSingleEvaluateNode())
                node = InputNode.INSTANCE;
            else if (tokenStream.isCurrentBeginBracket())
                return of(compileBracket(tokenStream));
            else if (tokenStream.isSingleUnaryOperator(isFirstNode)) {
                Token unaryOperatorToken = tokenStream.pop();
                return of(new Expression(new ConstNode(null), unaryOperatorToken.toOperator(true), compileValueNode(tokenStream, false).get()));
            }
        }
        if (isFirstNode && node == null)
            return of(InputNode.INSTANCE);
        if (node != null)
            while (tokenStream.hasTokens() && tokenStream.isCurrentSingleEvaluateNode()) {
                Token token = tokenStream.pop();
                node = new PropertyNode(node, token.getProperties());
                node.setPositionBegin(token.getPositionBegin());
                node.setPositionEnd(token.getPositionEnd());
            }
        return ofNullable(node);
    }

    private Optional<Node> compileTypeNode(TokenStream tokenStream) {
        Node node = null;
        if (tokenStream.hasTokens() && tokenStream.currentType() == Token.Type.WORD)
            node = new TypeNode(tokenStream.pop().getValue().toString());
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
