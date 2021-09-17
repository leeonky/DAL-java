package com.github.leeonky.dal.ast;

import java.util.Optional;

public interface ExpressionFactory {
    ExpressionFactory
            BRACKET_PROPERTY = NodeParser::compileBracketProperty,
            BEAN_PROPERTY = NodeParser::compileBeanProperty,
            EXPLICIT_PROPERTY = BEAN_PROPERTY.combine(BRACKET_PROPERTY);

    Optional<Node> tryFetch(NodeParser nodeParser, Node previous);

    default ExpressionFactory combine(ExpressionFactory another) {
        return (nodeParser, previous) -> {
            Optional<Node> optionalNode = tryFetch(nodeParser, previous);
            if (optionalNode.isPresent())
                return optionalNode;
            return another.tryFetch(nodeParser, previous);
        };
    }

    default NodeFactory withThis() {
        return nodeParser -> tryFetch(nodeParser, InputNode.INSTANCE);
    }
}
