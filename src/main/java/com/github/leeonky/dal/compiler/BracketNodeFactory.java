package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.BracketNode;
import com.github.leeonky.dal.ast.Node;

import static com.github.leeonky.dal.compiler.NodeFactory.createExpressionNodeFactory;

public class BracketNodeFactory implements NodeFactory {
    private static final NodeFactory expressionNodeFactory = createExpressionNodeFactory();

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        return nodeParser.compileNodeInBracket(() ->
                new BracketNode(expressionNodeFactory.fetchNode(nodeParser)).setPositionBegin(10));
    }

}
