package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

public interface NodeFactory {
    Node fetchNode(NodeParser nodeParser);

    default NodeFactory combine(NodeFactory nodeFactory) {
        return nodeParser -> {
            Node node = fetchNode(nodeParser);
            return node == null ? nodeFactory.fetchNode(nodeParser) : node;
        };
    }
}

