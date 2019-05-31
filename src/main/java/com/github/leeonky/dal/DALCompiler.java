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
    private Scanner scanner = new Scanner();

    public Node compile(SourceCode sourceCode) {
        return compileExpression(scanner.scan(sourceCode), InputNode.INSTANCE, null);
    }

    private Node compileExpression(TokenStream tokenStream, Node inputNode, BracketNode bracketNode) {
        Optional<Node> nodeOptional = compileValueNode(tokenStream);
        Node node;
        if (inputNode != null)
            node = nodeOptional.orElse(inputNode);
        else
            node = nodeOptional.get();
        while (tokenStream.hasTokens() && !tokenStream.isCurrentEndBracketAndTakeThenFinishBracket(bracketNode)) {
            node = new Expression(node, tokenStream.pop().toOperator(),
                    compileValueNode(tokenStream)
                            .orElseThrow(() -> new SyntaxException(tokenStream.getPosition(), "expression not finished")))
                    .adjustOperatorOrder();
        }
        return node;
    }

    private Optional<Node> compileValueNode(TokenStream tokenStream) {
        Node node = null;
        if (tokenStream.hasTokens()) {
            if (tokenStream.currentType() == Token.Type.CONST_VALUE)
                node = new ConstNode(tokenStream.pop().getConstValue());
            else if (tokenStream.isCurrentSingleEvaluateNode())
                node = InputNode.INSTANCE;
            else if (tokenStream.isCurrentBeginBracket()) {
                return of(compileBracket(tokenStream, node));
            }
        }
        if (node != null)
            while (tokenStream.hasTokens() && tokenStream.isCurrentSingleEvaluateNode()) {
                Token token = tokenStream.pop();
                node = new PropertyNode(node, token.getProperties());
                node.setPositionBegin(token.getPositionBegin());
                node.setPositionEnd(token.getPositionEnd());
            }
        return ofNullable(node);
    }

    private BracketNode compileBracket(TokenStream tokenStream, Node node) {
        Token bracketToken = tokenStream.pop();
        BracketNode bracketNode = new BracketNode();
        bracketNode.setNode(compileExpression(tokenStream, node, bracketNode));
        if (!bracketNode.isFinished())
            throw new SyntaxException(bracketToken.getPositionBegin(), "missed end bracket");
        return bracketNode;
    }
}
