package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.ParenthesesNode;

import static com.github.leeonky.dal.compiler.NodeFactory.createExpressionNodeFactory;

public class ParenthesesNodeFactory implements NodeFactory {
    private static final NodeFactory expressionNodeFactory = createExpressionNodeFactory();

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        return nodeParser.compileNodeInParentheses(() ->
                new ParenthesesNode(expressionNodeFactory.fetchNode(nodeParser)).setPositionBegin(10));
    }

}
