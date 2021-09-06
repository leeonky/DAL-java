package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.token.Token;

import java.util.Optional;

public interface NodeFactory {
    NodeFactory
            IDENTIFIER_PROPERTY = NodeParser::compileIdentifierProperty,
            PROPERTY = NodeFactory.IDENTIFIER_PROPERTY
                    .combine(ExpressionFactory.EXPLICIT_PROPERTY.withThis()),
            REGEX = nodeParser -> nodeParser.compileSingle(Token.Type.REGEX, Token::toRegexNode),
            CONST = nodeParser -> nodeParser.compileSingle(Token.Type.CONST_VALUE, Token::toConstNode),
            PARENTHESES = NodeParser::compileParentheses,
            SINGLE_EVALUABLE = NodeFactory.CONST.
                    combine(NodeFactory.PARENTHESES).
                    combine(NodeFactory.PROPERTY),
            OBJECT = NodeParser::compileObject,
            LIST = NodeParser::compileList;

    MandatoryNodeFactory
            OPERAND = NodeParser::compileOperand,
            EXPRESSION = nodeParser -> nodeParser.compileExpression(OPERAND.fetchNode(nodeParser)),
            RIGHT_OPERAND = REGEX.
                    combine(OBJECT).
                    combine(LIST).
                    combine(OPERAND);


    Optional<Node> fetch(NodeParser nodeParser);

    default NodeFactory combine(NodeFactory another) {
        return nodeParser -> {
            Optional<Node> optionalNode = fetch(nodeParser);
            if (optionalNode.isPresent())
                return optionalNode;
            return another.fetch(nodeParser);
        };
    }

    default MandatoryNodeFactory combine(MandatoryNodeFactory another) {
        return nodeParser -> fetch(nodeParser).orElseGet(() -> another.fetchNode(nodeParser));
    }
}
