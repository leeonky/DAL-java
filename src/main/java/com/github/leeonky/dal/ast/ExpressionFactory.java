package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.token.Token;

import java.util.Optional;

public interface ExpressionFactory {
    ExpressionFactory
            BRACKET_PROPERTY = NodeParser::compileBracketProperty,
            BEAN_PROPERTY = (nodeParser, instance) ->
                    nodeParser.compileSingle(Token.Type.PROPERTY, value -> value.toDotPropertyNode(instance)),
            EXPLICIT_PROPERTY = BEAN_PROPERTY.combine(BRACKET_PROPERTY);

    Optional<Node> fetch(NodeParser nodeParser, Node previous);

    default ExpressionFactory combine(ExpressionFactory another) {
        return (nodeParser, previous) -> {
            Optional<Node> optionalNode = fetch(nodeParser, previous);
            if (optionalNode.isPresent())
                return optionalNode;
            return another.fetch(nodeParser, previous);
        };
    }

    default NodeFactory withThis() {
        return nodeParser -> fetch(nodeParser, InputNode.INSTANCE);
    }
}
