package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

public class RightOperandNodeFactory implements NodeFactory {
    private final NodeFactory regexNodeFactory = NodeFactory.createRegexNodeFactory();
    private final NodeFactory objectNodeFactory = NodeFactory.createObjectNodeFactory();
    private final NodeFactory singleEvaluableNodeFactory = NodeFactory.createSingleEvaluableNodeFactory();

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        Node node = regexNodeFactory.fetchNode(nodeParser);
        if (node == null)
            node = objectNodeFactory.fetchNode(nodeParser);
        if (node == null)
            node = singleEvaluableNodeFactory.fetchNode(nodeParser);
        return node;
    }
}
