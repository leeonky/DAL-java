package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

class PropertyNodeFactory implements NodeFactory {
    private static final NodeFactory wordPropertyNodeFactory = NodeFactory.createWordPropertyNodeFactory();
    private static final NodeFactory explicitPropertyNodeFactory = NodeFactory.createExplicitPropertyNodeFactory();

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        //TODO support property chain
        Node node = explicitPropertyNodeFactory.fetchNode(nodeParser);
        if (node == null)
            node = wordPropertyNodeFactory.fetchNode(nodeParser);
        return node;
    }
}
