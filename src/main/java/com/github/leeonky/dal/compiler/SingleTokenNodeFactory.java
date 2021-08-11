package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.Token;

import java.util.function.Function;

abstract class SingleTokenNodeFactory implements NodeFactory {
    private final Token.Type tokenType;

    public SingleTokenNodeFactory(Token.Type constValue) {
        tokenType = constValue;
    }

    public static SingleTokenNodeFactory singleTokenNodeFactory(Token.Type type, Function<Object, Node> creator) {
        return new SingleTokenNodeFactory(type) {
            @Override
            protected Node createNode(NodeParser nodeParser, Object value) {
                return creator.apply(value);
            }
        };
    }

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        if (nodeParser.tokenStream.currentType() == tokenType) {
            Token token = nodeParser.tokenStream.pop();
            return createNode(nodeParser, token.getValue()).setPositionBegin(token.getPositionBegin());
        }
        return null;
    }

    protected abstract Node createNode(NodeParser nodeParser, Object value);
}
