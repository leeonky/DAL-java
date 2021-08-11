package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.NoMoreTokenException;

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
        try {
            return parsePropertyChain(nodeParser, nodeFactories.stream()
                    .map(factory -> factory.fetchNode(nodeParser))
                    .filter(Objects::nonNull).findFirst()
                    .orElseThrow(() -> noValueException(nodeParser)));
        } catch (NoMoreTokenException ignore) {
            throw noValueException(nodeParser);
        }
    }

    private SyntaxException noValueException(NodeParser nodeParser) {
        return new SyntaxException(nodeParser.tokenStream.getPosition(), "expect a value or expression");
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
