package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.ParenthesesNode;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;

import java.util.LinkedList;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.ast.PropertyNode.Type.DOT;
import static com.github.leeonky.dal.token.Token.Type.*;

public class NodeParser {
    //TODO to be private
    public final LinkedList<Token> operators = new LinkedList<>();
    //TODO to be private
    final TokenStream tokenStream;
    private int parenthesisCount = 0;
    private Node thisNode = InputNode.INSTANCE;

    public NodeParser(TokenStream tokenStream) {
        this.tokenStream = tokenStream;
    }

    //TODO logic for clear
    public Node setThis(Node node) {
        return thisNode = node;
    }

    public Node getThisNode() {
        return thisNode;
    }

    public Node compileParenthesesNode() {
        return tokenStream.parseBetween(OPENING_PARENTHESIS, CLOSING_PARENTHESIS, ')', () -> {
            try {
                parenthesisCount++;
                return new ParenthesesNode(NodeFactories.EXPRESSION.fetchNode(this));
            } finally {
                parenthesisCount--;
            }
        });
    }

    public boolean isInParentheses() {
        return parenthesisCount == 0;
    }

    public Node compileBracketPropertyNode() {
        return tokenStream.parseBetween(OPENING_BRACKET, CLOSING_BRACKET, ']', () -> {
            if (tokenStream.hasTokens())
                return new PropertyNode(getThisNode(), tokenStream.pop().getPropertyOrIndex(), BRACKET);
            throw new SyntaxException(tokenStream.getPosition(), "should given one property or array index in `[]`");
        });
    }

    //TODO refactor
    public Node compileIdentifierProperty() {
        if (tokenStream.currentType() == IDENTIFIER) {
            Token token = tokenStream.pop();
            String[] names = ((String) token.getValue()).split("\\.");
            Node node = new PropertyNode(getThisNode(), names[0], PropertyNode.Type.IDENTIFIER)
                    .setPositionBegin(token.getPositionBegin());
            for (int i = 1; i < names.length; i++)
                node = new PropertyNode(node, names[i], DOT)
                        .setPositionBegin(node.getPositionBegin() + names[i - 1].length() + 1);
            return node;
        }
        return null;
    }
}
