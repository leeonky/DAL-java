package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.DALOperator.Matcher;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.interpreter.NodeBase;
import com.github.leeonky.interpreter.Token;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.ast.AssertionFailure.*;
import static java.lang.String.format;

public abstract class DALNode extends NodeBase<RuntimeContextBuilder.DALRuntimeContext, DALNode> {

    public static SymbolNode symbolNode(Token token) {
        return new SymbolNode(token.getContent(), SymbolNode.Type.SYMBOL);
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

    public static Function<Token, DALNode> constNode(Function<Token, ?> function) {
        return token -> new ConstNode(function.apply(token));
    }

    public Data evaluateData(RuntimeContextBuilder.DALRuntimeContext context) {
        return context.wrap(evaluate(context));
    }

    @Override
    public Object evaluate(RuntimeContextBuilder.DALRuntimeContext context) {
        return evaluateData(context).getInstance();
    }

    public boolean verify(DALNode actualNode, DALOperator.Equal operator, RuntimeContextBuilder.DALRuntimeContext context) {
        return assertEquals(evaluateData(context), evaluateAndWrapperFailureMessage(actualNode, context),
                getPositionBegin());
    }

    public boolean verify(DALNode actualNode, Matcher operator, RuntimeContextBuilder.DALRuntimeContext context) {
        Data expected = evaluateData(context);
        Data actual = evaluateAndWrapperFailureMessage(actualNode, context);
        if (expected.isNull())
            return assertMatchNull(actual, actualNode.getPositionBegin());

        invalidTypeToMatchValue(String.class, actual, Number.class, expected, operator);
        invalidTypeToMatchValue(String.class, actual, Boolean.class, expected, operator);

        invalidTypeToMatchValue(Number.class, actual, String.class, expected, operator);
        invalidTypeToMatchValue(Boolean.class, actual, String.class, expected, operator);
        return assertMatch(expected, actual, getPositionBegin(), context.getConverter());
    }

    private Data evaluateAndWrapperFailureMessage(DALNode actualNode, RuntimeContextBuilder.DALRuntimeContext context) {
        try {
            return actualNode.evaluateData(context);
        } catch (AssertionFailure assertionFailure) {
            throw assertionFailure.multiPosition(getPositionBegin(), Position.Type.CHAR);
        }
    }

    public abstract String inspect();

    private void invalidTypeToMatchValue(Class<?> actualType, Data actual, Class<?> expectedType, Data expected, Matcher operator) {
        if (actualType.isInstance(actual.getInstance()) && expectedType.isInstance(expected.getInstance()))
            throw new RuntimeException(format("Cannot compare between %sand %s", actual.inspect(), expected.inspect()),
                    operator.getPosition());
    }

    public DALNode avoidListMapping() {
        return this;
    }

    public Object getRootSymbolName() {
        return null;
    }

    public List<Object> propertyChain() {
        throw new IllegalStateException();
    }
}
