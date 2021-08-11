package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.token.Token;

import static com.github.leeonky.dal.compiler.SingleTokenNodeFactory.singleTokenNodeFactory;

public interface NodeFactory {

    static NodeFactory createConstNodeFactory() {
        return singleTokenNodeFactory(Token.Type.CONST_VALUE, ConstNode::new);
    }

    static NodeFactory createSingleEvaluableNodeFactory() {
        return new SingleEvaluableNodeFactory();
    }

    static NodeFactory createRegexNodeFactory() {
        return singleTokenNodeFactory(Token.Type.REGEX, regex -> new RegexNode((String) regex));
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
        return nodeParser -> ExpressionFactory.INSTANCE.parseExpression(nodeParser, nodeFactory.fetchNode(nodeParser));
    }

    static NodeFactory createBracketNodeFactory() {
        NodeFactory expressionNodeFactory = createExpressionNodeFactory();
        return nodeParser -> {
            if (nodeParser.tokenStream.isCurrentBeginBracket()) {
                nodeParser.tokenStream.pop();
                BracketNode bracketNode = new BracketNode();

                //TODO move to constructor
                //TODO fetchNode should not be null
                bracketNode.setNode(expressionNodeFactory.fetchNode(nodeParser));

                //TODO does not needed
                if (nodeParser.tokenStream.currentType() == Token.Type.END_BRACKET)
                    nodeParser.tokenStream.pop();
                return bracketNode.setPositionBegin(10);
            }
            return null;
        };
    }

    Node fetchNode(NodeParser nodeParser);
}

