package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.ast.RegexNode;
import com.github.leeonky.dal.token.Token;

public class NodeFactories {
    public static final CombinedNodeFactory EXPLICIT_PROPERTY = new CombinedNodeFactory();
    public static final CombinedNodeFactory PROPERTY = new CombinedNodeFactory();
    public static final NodeFactory BRACKET_PROPERTY = NodeParser::compileBracketPropertyNode;
    public static final SingleTokenNodeFactory BEAN_PROPERTY = new SingleTokenNodeFactory(Token.Type.PROPERTY) {
        @Override
        protected Node createNode(NodeParser nodeParser, Object value) {
            return new PropertyNode(nodeParser.getThisNode(), value);
        }
    };
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
    public static final NodeFactory IDENTIFIER = NodeParser::compileIdentifierProperty;
    public static final SingleEvaluableNodeFactory SINGLE_EVALUABLE = new SingleEvaluableNodeFactory();
    public static final CombinedNodeFactory RIGHT_OPERAND = new CombinedNodeFactory();
    public static final NodeFactory EXPRESSION = nodeParser -> ExpressionParser.INSTANCE.apply(nodeParser,
            SINGLE_EVALUABLE.fetchNode(nodeParser));
    public static final NodeFactory PARENTHESES = NodeParser::compileParenthesesNode;

    static {
        EXPLICIT_PROPERTY.combine(BEAN_PROPERTY, BRACKET_PROPERTY);

        PROPERTY.combine(IDENTIFIER, EXPLICIT_PROPERTY);

        SINGLE_EVALUABLE.combine(CONST, PARENTHESES, PROPERTY);

        RIGHT_OPERAND.combine(REGEX, OBJECT, LIST, SINGLE_EVALUABLE);
    }
}
