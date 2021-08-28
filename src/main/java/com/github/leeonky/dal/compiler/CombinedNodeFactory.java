package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static java.util.Arrays.asList;

class CombinedNodeFactory implements NodeFactory {
    private final List<NodeFactory> nodeFactories = new ArrayList<>();

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        return nodeFactories.stream().map(factory -> factory.fetchNode(nodeParser))
                .filter(Objects::nonNull)
                .findFirst()
                .orElse(null);
    }

    public void combine(NodeFactory... factories) {
        nodeFactories.addAll(asList(factories));
    }
}
