package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.RegexNode;
import com.github.leeonky.dal.token.Token;

public interface NodeFactory {

    NodeFactory
            BRACKET_PROPERTY = NodeParser::compileBracketPropertyNode,
            BEAN_PROPERTY = nodeParser -> nodeParser.compileSingle(Token.Type.PROPERTY, nodeParser::createProperty),
            EXPLICIT_PROPERTY = BEAN_PROPERTY.combine(BRACKET_PROPERTY),
            IDENTIFIER = NodeParser::compileIdentifierProperty,
            PROPERTY = IDENTIFIER.combine(EXPLICIT_PROPERTY),
            REGEX = nodeParser -> nodeParser.compileSingle(Token.Type.REGEX, value -> new RegexNode((String) value)),
            CONST = nodeParser -> nodeParser.compileSingle(Token.Type.CONST_VALUE, ConstNode::new),
            OPERAND = NodeParser::compileOperand,
            EXPRESSION = nodeParser -> nodeParser.compileExpression(OPERAND.fetchNode(nodeParser)),
            PARENTHESES = NodeParser::compileParenthesesNode,
            SINGLE_EVALUABLE = CONST.combine(PARENTHESES).combine(PROPERTY),
            OBJECT = NodeParser::compileObject,
            LIST = NodeParser::compileList,
            RIGHT_OPERAND = REGEX.combine(OBJECT).combine(LIST).combine(OPERAND);

    Node fetchNode(NodeParser nodeParser);

    default NodeFactory combine(NodeFactory nodeFactory) {
        return nodeParser -> {
            Node node = fetchNode(nodeParser);
            return node == null ? nodeFactory.fetchNode(nodeParser) : node;
        };
    }
}

