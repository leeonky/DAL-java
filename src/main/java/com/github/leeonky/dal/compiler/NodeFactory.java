package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.ast.RegexNode;
import com.github.leeonky.dal.token.Token;

import static com.github.leeonky.dal.ast.PropertyNode.Type.IDENTIFIER;

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
        return new PropertyNodeFactory();
    }

    static NodeFactory createBracketPropertyNodeFactory() {
        return new BracketPropertyNodeFactory();
    }

    static NodeFactory createBeanPropertyNodeFactory() {
        return new SingleTokenNodeFactory(Token.Type.PROPERTY) {
            @Override
            protected Node createNode(NodeParser nodeParser, Object value) {
                return new PropertyNode(nodeParser.getThisNode(), value);
            }
        };
    }

    static NodeFactory createWordPropertyNodeFactory() {
        return new SingleTokenNodeFactory(Token.Type.WORD) {
            @Override
            protected Node createNode(NodeParser nodeParser, Object value) {
                return new PropertyNode(nodeParser.getThisNode(), value, IDENTIFIER);
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

    static NodeFactory createObjectNodeFactory() {
        return new ObjectNodeFactory();
    }

    static NodeFactory createRightOperandNodeFactory() {
        return new RightOperandNodeFactory();
    }

    static NodeFactory createExplicitPropertyNodeFactory() {
        return new ExplicitPropertyNodeFactory();
    }

    Node fetchNode(NodeParser nodeParser);
}

