package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

class PropertyNodeFactory implements NodeFactory {
    private static final NodeFactory beanPropertyNodeFactory = NodeFactory.createBeanPropertyNodeFactory();
    private static final NodeFactory bracketPropertyNodeFactory = NodeFactory.createBracketPropertyNodeFactory();
    private static final NodeFactory wordPropertyNodeFactory = NodeFactory.createWordPropertyNodeFactory();

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        Node node = beanPropertyNodeFactory.fetchNode(nodeParser);
        if (node == null)
            node = bracketPropertyNodeFactory.fetchNode(nodeParser);
        if (node == null)
            node = wordPropertyNodeFactory.fetchNode(nodeParser);
        return node;
    }
}
