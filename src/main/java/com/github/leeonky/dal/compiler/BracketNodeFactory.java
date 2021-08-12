package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.BracketNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.Token;

import static com.github.leeonky.dal.compiler.NodeFactory.createExpressionNodeFactory;

public class BracketNodeFactory implements NodeFactory {
    private static final NodeFactory expressionNodeFactory = createExpressionNodeFactory();

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        if (nodeParser.tokenStream.isCurrentBeginBracket()) {
            nodeParser.tokenStream.pop();

            //TODO move to constructor
            BracketNode bracketNode = new BracketNode();
            bracketNode.setNode(expressionNodeFactory.fetchNode(nodeParser));

            if (!nodeParser.tokenStream.hasTokens())
                throw new SyntaxException(nodeParser.tokenStream.getPosition(), "missed end bracket");

            if (nodeParser.tokenStream.currentType() == Token.Type.END_BRACKET)
                nodeParser.tokenStream.pop();
            else
                throw new SyntaxException(nodeParser.tokenStream.getPosition(), "unexpected token, ')' expected");

            //TODO does not needed
            bracketNode.finishBracket();
            return bracketNode.setPositionBegin(10);
        }
        return null;
    }
}
