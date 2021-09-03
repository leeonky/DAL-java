package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.token.Token;

import static java.util.Optional.ofNullable;

public interface NodeFactory {
    ExpressionFactory
            BRACKET_PROPERTY = NodeParser::compileBracketProperty,
            BEAN_PROPERTY = (nodeParser, instanceNode) ->
                    nodeParser.compileSingle(Token.Type.PROPERTY, value -> value.toDotPropertyNode(instanceNode)),
            EXPLICIT_PROPERTY = BEAN_PROPERTY.combine(BRACKET_PROPERTY);

    NodeFactory
            IDENTIFIER_PROPERTY = NodeParser::compileIdentifierProperty,
            PROPERTY = IDENTIFIER_PROPERTY.combine(EXPLICIT_PROPERTY.inThis()),
            REGEX = nodeParser -> nodeParser.compileSingle(Token.Type.REGEX, Token::toRegexNode),
            CONST = nodeParser -> nodeParser.compileSingle(Token.Type.CONST_VALUE, Token::toConstNode),
            OPERAND = NodeParser::compileOperand,
            EXPRESSION = nodeParser -> nodeParser.compileExpression(OPERAND.fetchNode(nodeParser)),
            PARENTHESES = NodeParser::compileParentheses,
            SINGLE_EVALUABLE = CONST.combine(PARENTHESES).combine(PROPERTY),
            OBJECT = NodeParser::compileObject,
            LIST = NodeParser::compileList,
            RIGHT_OPERAND = REGEX.combine(OBJECT).combine(LIST).combine(OPERAND);


    Node fetchNode(NodeParser nodeParser);

    default NodeFactory combine(NodeFactory nodeFactory) {
        return nodeParser -> ofNullable(fetchNode(nodeParser)).orElseGet(() -> nodeFactory.fetchNode(nodeParser));
    }
}

