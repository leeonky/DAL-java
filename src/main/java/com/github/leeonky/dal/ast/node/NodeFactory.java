package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.util.TextUtil;
import com.github.leeonky.interpreter.SyntaxException;
import com.github.leeonky.interpreter.Token;
import com.github.leeonky.util.NumberParser;

import java.math.BigInteger;
import java.util.List;
import java.util.stream.Collectors;

public class NodeFactory {
    private static final NumberParser numberParser = new NumberParser();

    public static DALNode stringSymbol(DALNode dalNode) {
        return new SymbolNode(((ConstValueNode) dalNode).getValue(), SymbolNode.Type.STRING)
                .setPositionBegin(dalNode.getPositionBegin());
    }

    public static DALNode numberSymbol(DALNode dalNode) {
        return new SymbolNode(((ConstValueNode) dalNode).getValue(), SymbolNode.Type.NUMBER)
                .setPositionBegin(dalNode.getPositionBegin());
    }

    public static SymbolNode symbolNode(Token token) {
        return new SymbolNode(token.getContent(), SymbolNode.Type.SYMBOL);
    }

    public static SymbolNode metaSymbolNode(Token token) {
        return new MetaSymbolNode(token.getContent());
    }

    public static SchemaComposeNode schemas(List<DALNode> nodes) {
        return new SchemaComposeNode(nodes.stream().map(SchemaNode.class::cast).collect(Collectors.toList()), false);
    }

    public static SchemaComposeNode elementSchemas(List<DALNode> nodes) {
        return new SchemaComposeNode(nodes.stream().map(SchemaNode.class::cast).collect(Collectors.toList()), true);
    }

    public static SchemaNode schema(Token token) {
        return (SchemaNode) new SchemaNode(token.getContent()).setPositionBegin(token.getPosition());
    }

    public static DALNode bracketSymbolNode(DALNode node) {
        return new SymbolNode(((ConstValueNode) node).getValue(), SymbolNode.Type.BRACKET);
    }

    public static DALNode parenthesesNode(DALNode node) {
        return new ParenthesesNode(node);
    }

    public static DALNode constString(List<Character> characters) {
        return new ConstValueNode(TextUtil.join(characters));
    }

    public static DALNode relaxString(Token token) {
        return new ConstValueNode(token.getContent().trim());
    }

    public static DALNode regex(List<Character> characters) {
        return new RegexNode(TextUtil.join(characters));
    }

    public static DALNode constTrue(String token) {
        return new ConstValueNode(true);
    }

    public static DALNode constFalse(String token) {
        return new ConstValueNode(false);
    }

    public static DALNode constNull(String token) {
        return new ConstValueNode(null);
    }

    public static ConstValueNode constNumber(Token token) {
        return new ConstValueNode(numberParser.parse(token.getContent()));
    }

    public static ConstValueNode constInteger(Token token) {
        Number number = numberParser.parse(token.getContent());
        if (number != null) {
            Class<? extends Number> type = number.getClass();
            if (type.equals(Integer.class) || type.equals(Long.class) || type.equals(Short.class)
                    || type.equals(Byte.class) || type.equals(BigInteger.class)) {
                return new ConstValueNode(number);
            }
        }
        throw new SyntaxException("expect an integer", token.getPosition());
    }

    public static DALNode createVerificationGroup(List<DALNode> list) {
        if (list.size() == 1)
            return list.get(0);
        return new GroupExpression(list);
    }

    public static DALNode constRemarkNode(DALNode constNode, DALNode parentheses) {
        return new ConstRemarkNode(constNode, parentheses);
    }

    public static DALNode dataRemarkNode(List<Character> characters) {
        return new DataRemarkNode(TextUtil.join(characters).trim());
    }
}
