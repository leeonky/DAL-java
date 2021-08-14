package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.token.Token;

class BracketPropertyNodeFactory implements NodeFactory {

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        if (nodeParser.tokenStream.currentType() == Token.Type.OPENING_BRACKET) {
            nodeParser.tokenStream.pop();
            if (!nodeParser.tokenStream.hasTokens())
                throw new SyntaxException(nodeParser.tokenStream.getPosition(), "should end with `]`");

            PropertyNode propertyNode = new PropertyNode(nodeParser.getThisNode(),
                    nodeParser.tokenStream.pop().getPropertyOrIndex(), true);

            if (!nodeParser.tokenStream.hasTokens())
                throw new SyntaxException(nodeParser.tokenStream.getPosition(), "should end with `]`");

            if (nodeParser.tokenStream.currentType() != Token.Type.CLOSING_BRACKET)
                throw new SyntaxException(nodeParser.tokenStream.getPosition(),
                        "should given one property or array index in `[]`");
            nodeParser.tokenStream.pop();
            return propertyNode;
        }
        return null;
    }
}
