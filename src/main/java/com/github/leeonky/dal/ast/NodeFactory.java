package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.token.Token;

import java.util.Optional;
import java.util.stream.Stream;

public interface NodeFactory {
    NodeFactory
            IDENTIFIER_PROPERTY = NodeParser::compileIdentifierProperty,
            PROPERTY = NodeFactory.IDENTIFIER_PROPERTY.combines(ExpressionFactory.EXPLICIT_PROPERTY.withThis()),
            REGEX = nodeParser -> nodeParser.compileSingle(Token.Type.REGEX, Token::toRegexNode),
            CONST = nodeParser -> nodeParser.compileSingle(Token.Type.CONST_VALUE, Token::toConstNode),
            PARENTHESES = NodeParser::compileParentheses,
            SINGLE_EVALUABLE = NodeFactory.CONST.combines(NodeFactory.PARENTHESES, NodeFactory.PROPERTY),
            OBJECT = NodeParser::compileObject,
            LIST = NodeParser::compileList,
            MATCH_ALL = NodeParser::matchAll,
            JUDGEMENT = REGEX.combines(OBJECT, LIST, MATCH_ALL),
            LIST_TAIL = NodeParser::listTail;

    MandatoryNodeFactory
            OPERAND = NodeParser::compileOperand,
            EXPRESSION = nodeParser -> nodeParser.compileExpression(OPERAND.fetch(nodeParser)),
            JUDGEMENT_OR_OPERAND = JUDGEMENT.combine(OPERAND),
            CALCULATION_EXPRESSION = nodeParser -> nodeParser.compileCalculationExpression(OPERAND.fetch(nodeParser)),
            JUDGEMENT_OR_CALCULATION_EXPRESSION = JUDGEMENT.combine(CALCULATION_EXPRESSION);


    Optional<Node> tryFetch(NodeParser nodeParser);

    default NodeFactory combine(NodeFactory another) {
        return nodeParser -> {
            Optional<Node> optionalNode = tryFetch(nodeParser);
            if (optionalNode.isPresent())
                return optionalNode;
            return another.tryFetch(nodeParser);
        };
    }

    default NodeFactory combines(NodeFactory... others) {
        return Stream.of(others).reduce(this, NodeFactory::combine);
    }

    default MandatoryNodeFactory combine(MandatoryNodeFactory another) {
        return nodeParser -> tryFetch(nodeParser).orElseGet(() -> another.fetch(nodeParser));
    }
}
