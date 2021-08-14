package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

class ExplicitPropertyNodeFactory implements NodeFactory {
    private static final NodeFactory beanPropertyNodeFactory = NodeFactory.createBeanPropertyNodeFactory();
    private static final NodeFactory bracketPropertyNodeFactory = NodeFactory.createBracketPropertyNodeFactory();

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        Node node = beanPropertyNodeFactory.fetchNode(nodeParser);
        if (node == null)
            node = bracketPropertyNodeFactory.fetchNode(nodeParser);
        return node;
    }
}
