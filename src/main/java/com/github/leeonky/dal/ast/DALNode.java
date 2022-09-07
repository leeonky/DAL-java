package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.DALOperator.Equal;
import com.github.leeonky.dal.ast.DALOperator.Matcher;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.interpreter.NodeBase;
import com.github.leeonky.interpreter.SyntaxException;
import com.github.leeonky.interpreter.Token;
import com.github.leeonky.util.NumberParser;

import java.math.BigInteger;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.AssertionFailure.*;
import static java.lang.String.format;

public abstract class DALNode extends NodeBase<DALRuntimeContext, DALNode> {
    private static final NumberParser numberParser = new NumberParser();

    public static DALNode stringSymbol(DALNode dalNode) {
        return new SymbolNode(((ConstNode) dalNode).getValue(), SymbolNode.Type.STRING);
    }

    public static DALNode numberSymbol(DALNode dalNode) {
        return new SymbolNode(((ConstNode) dalNode).getValue(), SymbolNode.Type.NUMBER);
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
        return new DALExpression(null, new DALOperator.Parentheses(), node);
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

    public Data evaluateData(DALRuntimeContext context) {
        return context.wrap(evaluate(context));
    }

    @Override
    public Object evaluate(DALRuntimeContext context) {
        return evaluateData(context).getInstance();
    }

    public boolean verifyBy(DALNode expected, Equal operator, DALRuntimeContext context) {
        return expected.verify(this, operator, context);
    }

    public boolean verifyBy(DALNode expected, DALOperator.Matcher operator, DALRuntimeContext context) {
        return expected.verify(this, operator, context);
    }

    public boolean verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        return verify(actualNode.evaluateData(context), operator, context, actualNode);
    }

    public boolean verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        return verify(actualNode.evaluateData(context), operator, context, actualNode);
    }

    protected boolean verify(Data actual, Equal operator, DALRuntimeContext context, DALNode actualNode) {
        return assertEquals(evaluateData(context), actual, getPositionBegin());
    }

    protected boolean verify(Data actual, Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        Data expected = evaluateData(context);
        if (expected.isNull())
            return assertMatchNull(actual, actualNode.getPositionBegin());

        invalidTypeToMatchValue(String.class, actual, Number.class, expected, operator);
        invalidTypeToMatchValue(String.class, actual, Boolean.class, expected, operator);

        invalidTypeToMatchValue(Number.class, actual, String.class, expected, operator);
        invalidTypeToMatchValue(Boolean.class, actual, String.class, expected, operator);
        return assertMatch(expected, actual, getPositionBegin(), context.getNumberType());
    }

    public abstract String inspect();

    private void invalidTypeToMatchValue(Class<?> actualType, Data actual, Class<?> expectedType, Data expected,
                                         Matcher operator) {
        if (actualType.isInstance(actual.getInstance()) && expectedType.isInstance(expected.getInstance()))
            throw new RuntimeException(format("Cannot compare between %sand %s", actual.inspect(), expected.inspect()).trim(),
                    operator.getPosition());
    }

    public Object getRootSymbolName() {
        return null;
    }

    public List<Object> propertyChain() {
        throw new IllegalStateException();
    }

    public Stream<Object> collectFields(Data data) {
        return Stream.of(data.firstFieldFromAlias(getRootSymbolName()));
    }
}
