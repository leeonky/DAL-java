package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.token.Token;

import java.util.Optional;

public interface OptionalExpressionFactory {
    OptionalExpressionFactory
            BRACKET_PROPERTY = NodeParser::compileBracketProperty,
            BEAN_PROPERTY = (nodeParser, instance) ->
                    nodeParser.compileSingle2(Token.Type.PROPERTY, value -> value.toDotPropertyNode(instance)),
            EXPLICIT_PROPERTY = BEAN_PROPERTY.combine(BRACKET_PROPERTY);

    Optional<Node> fetch(NodeParser nodeParser, Node previous);

    @Deprecated
    default ExpressionFactory returnInstance() {
        return (nodeParser, previous) -> fetch(nodeParser, previous).orElse(null);
    }

    default OptionalExpressionFactory combine(OptionalExpressionFactory another) {
        return (nodeParser, previous) -> {
            Optional<Node> optionalNode = fetch(nodeParser, previous);
            if (optionalNode.isPresent())
                return optionalNode;
            return another.fetch(nodeParser, previous);
        };
    }
}
