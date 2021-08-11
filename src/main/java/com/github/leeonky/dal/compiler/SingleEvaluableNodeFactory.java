package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.List;
import java.util.Objects;

import static java.util.Arrays.asList;

class SingleEvaluableNodeFactory implements NodeFactory {
    private final NodeFactory propertyNodeFactory = NodeFactory.createPropertyNodeFactory();
    private final List<NodeFactory> nodeFactories = asList(
            NodeFactory.createConstNodeFactory(),
            propertyNodeFactory
    );

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        return parsePropertyChain(nodeParser, nodeFactories.stream()
                .map(factory -> factory.fetchNode(nodeParser))
                .filter(Objects::nonNull).findFirst().orElse(null));
    }

    private Node parsePropertyChain(NodeParser nodeParser, Node node) {
        if (nodeParser.setThis(node) != null && nodeParser.tokenStream.hasTokens()) {
            Node next = propertyNodeFactory.fetchNode(nodeParser);
            if (next != null)
                return parsePropertyChain(nodeParser, next);
        }
        return node;
    }
}
