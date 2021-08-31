package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.ast.RegexNode;
import com.github.leeonky.dal.token.Token;

import static com.github.leeonky.dal.compiler.ExpressionParser.EXPRESSION_PARSER;

public class NodeFactories {
    public static final SingleTokenNodeFactory BEAN_PROPERTY = new SingleTokenNodeFactory(Token.Type.PROPERTY) {
        @Override
        protected Node createNode(NodeParser nodeParser, Object value) {
            return new PropertyNode(nodeParser.getThisNode(), value);
        }
    };
    public static final NodeFactory BRACKET_PROPERTY = NodeParser::compileBracketPropertyNode;
    public static final NodeFactory EXPLICIT_PROPERTY = BEAN_PROPERTY.combine(BRACKET_PROPERTY);
    public static final NodeFactory IDENTIFIER = NodeParser::compileIdentifierProperty;
    public static final NodeFactory PROPERTY = IDENTIFIER.combine(EXPLICIT_PROPERTY);
    public static final SingleTokenNodeFactory REGEX = new SingleTokenNodeFactory(Token.Type.REGEX) {
        @Override
        protected Node createNode(NodeParser nodeParser, Object value) {
            return new RegexNode((String) value);
        }
    };
    public static final ObjectNodeFactory OBJECT = new ObjectNodeFactory();
    public static final ListNodeFactory LIST = new ListNodeFactory();
    public static final SingleTokenNodeFactory CONST = new SingleTokenNodeFactory(Token.Type.CONST_VALUE) {
        @Override
        protected Node createNode(NodeParser nodeParser, Object value) {
            return new ConstNode(value);
        }
    };
    public static final OperandNodeFactory OPERAND = new OperandNodeFactory();
    public static final NodeFactory RIGHT_OPERAND = REGEX.combine(OBJECT).combine(LIST).combine(OPERAND);
    public static final NodeFactory PARENTHESES = NodeParser::compileParenthesesNode;
    public static final NodeFactory SINGLE_EVALUABLE = CONST.combine(PARENTHESES).combine(PROPERTY);
    public static final NodeFactory EXPRESSION = nodeParser -> EXPRESSION_PARSER.apply(nodeParser,
            OPERAND.fetchNode(nodeParser));
}
