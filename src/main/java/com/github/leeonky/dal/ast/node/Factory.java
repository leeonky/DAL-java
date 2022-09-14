package com.github.leeonky.dal.ast.node;

import com.github.leeonky.interpreter.SyntaxException;
import com.github.leeonky.interpreter.Token;
import com.github.leeonky.util.NumberParser;

import java.math.BigInteger;
import java.util.List;
import java.util.stream.Collectors;

public class Factory {
    private static final NumberParser numberParser = new NumberParser();

    public static DALNode stringSymbol(DALNode dalNode) {
        return new SymbolNode(((ConstNode) dalNode).getValue(), SymbolNode.Type.STRING)
                .setPositionBegin(dalNode.getPositionBegin());
    }

    public static DALNode numberSymbol(DALNode dalNode) {
        return new SymbolNode(((ConstNode) dalNode).getValue(), SymbolNode.Type.NUMBER)
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
        return new SymbolNode(((ConstNode) node).getValue(), SymbolNode.Type.BRACKET);
    }

    public static DALNode parenthesesNode(DALNode node) {
        return new DALExpression(null, com.github.leeonky.dal.ast.opt.Factory.parentheses(), node);
    }

    public static DALNode constString(List<DALNode> nodes) {
        return new ConstNode(getString(nodes));
    }

    public static DALNode relaxString(Token token) {
        return new ConstNode(token.getContent().trim());
    }

    public static DALNode regex(List<DALNode> nodes) {
        return new RegexNode(getString(nodes));
    }

    private static String getString(List<DALNode> nodes) {
        return nodes.stream().map(ConstNode.class::cast).map(ConstNode::getValue)
                .map(Object::toString).collect(Collectors.joining());
    }

    public static DALNode constTrue(String token) {
        return new ConstNode(true);
    }

    public static DALNode constFalse(String token) {
        return new ConstNode(false);
    }

    public static DALNode constNull(String token) {
        return new ConstNode(null);
    }

    public static ConstNode constNumber(Token token) {
        return new ConstNode(numberParser.parse(token.getContent()));
    }

    public static ConstNode constInteger(Token token) {
        Number number = numberParser.parse(token.getContent());
        if (number != null) {
            Class<? extends Number> type = number.getClass();
            if (type.equals(Integer.class) || type.equals(Long.class) || type.equals(Short.class)
                    || type.equals(Byte.class) || type.equals(BigInteger.class)) {
                return new ConstNode(number);
            }
        }
        throw new SyntaxException("expect an integer", token.getPosition());
    }

    public static DALNode createVerificationGroup(List<DALNode> list) {
        if (list.size() == 1)
            return list.get(0);
        return new GroupExpression(list);
    }
}
