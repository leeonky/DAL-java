package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.ast.RegexNode;
import com.github.leeonky.dal.token.Token;

public interface NodeFactory {

    static NodeFactory createConstNodeFactory() {
        return new SingleTokenNodeFactory(Token.Type.CONST_VALUE) {
            @Override
            protected Node createNode(NodeParser nodeParser, Object value) {
                return new ConstNode(value);
            }
        };
    }

    static NodeFactory createSingleEvaluableNodeFactory() {
        return new SingleEvaluableNodeFactory();
    }

    static NodeFactory createRegexNodeFactory() {
        return new SingleTokenNodeFactory(Token.Type.REGEX) {
            @Override
            protected Node createNode(NodeParser nodeParser, Object value) {
                return new RegexNode((String) value);
            }
        };
    }

    static NodeFactory createPropertyNodeFactory() {
        return new SingleTokenNodeFactory(Token.Type.PROPERTY) {
            @Override
            protected Node createNode(NodeParser nodeParser, Object value) {
                return new PropertyNode(nodeParser.getThisNode(), value);
            }
        };
    }

    static NodeFactory createExpressionNodeFactory() {
        NodeFactory nodeFactory = NodeFactory.createSingleEvaluableNodeFactory();
        return nodeParser -> ExpressionParser.INSTANCE.apply(nodeParser, nodeFactory.fetchNode(nodeParser));
    }

    static NodeFactory createParenthesesNodeFactory() {
        return new ParenthesesNodeFactory();
    }

    Node fetchNode(NodeParser nodeParser);
}

