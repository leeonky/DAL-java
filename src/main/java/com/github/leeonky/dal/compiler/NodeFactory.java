package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.ast.RegexNode;
import com.github.leeonky.dal.token.Token;

import static com.github.leeonky.dal.ast.PropertyNode.Type.DOT;
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

    //TODO position
    //TODO refactor
    static NodeFactory createIdentifierPropertyNodeFactory() {
        return nodeParser -> {
            if (nodeParser.tokenStream.currentType() == Token.Type.IDENTIFIER) {
                Token token = nodeParser.tokenStream.pop();
                String[] names = ((String) token.getValue()).split("\\.");
                Node node = new PropertyNode(nodeParser.getThisNode(), names[0], IDENTIFIER)
                        .setPositionBegin(token.getPositionBegin());
                for (int i = 1; i < names.length; i++)
                    node = new PropertyNode(node, names[i], DOT)
                            .setPositionBegin(node.getPositionBegin() + names[i - 1].length() + 1);
                return node;
            }
            return null;
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

    static NodeFactory createListNodeFactory() {
        return new ListNodeFactory();
    }

    Node fetchNode(NodeParser nodeParser);
}

