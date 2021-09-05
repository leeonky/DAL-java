package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.token.Token;

import java.util.Optional;

import static java.util.Optional.ofNullable;

//TODO return optional
public interface NodeFactory {

    NodeFactory
            IDENTIFIER_PROPERTY = NodeParser::compileIdentifierProperty,
            PROPERTY = IDENTIFIER_PROPERTY.combine(OptionalExpressionFactory.EXPLICIT_PROPERTY.returnInstance().inThis()),
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
        return nodeParser -> fetchNodeOptional(nodeParser).orElseGet(() -> nodeFactory.fetchNode(nodeParser));
    }

    @Deprecated
    default Optional<Node> fetchNodeOptional(NodeParser nodeParser) {
        return ofNullable(fetchNode(nodeParser));
    }
}
