package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.token.Token.Type.CLOSING_BRACKET;
import static com.github.leeonky.dal.token.Token.Type.OPENING_BRACKET;

class BracketPropertyNodeFactory implements NodeFactory {

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        return nodeParser.tokenStream.parseBetween(OPENING_BRACKET, CLOSING_BRACKET, ']', () -> {
            if (nodeParser.tokenStream.hasTokens())
                return new PropertyNode(nodeParser.getThisNode(),
                        nodeParser.tokenStream.pop().getPropertyOrIndex(), BRACKET);
            throw new SyntaxException(nodeParser.tokenStream.getPosition(), "should end with `]`");
        });
    }
}
