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

    public static DALNode elementSchemas(Token token, DALNode node) {
        return ((NodeCollection) node).toSchemaComposeNode(token.getPosition());
    }

    public static SchemaNode schema(Token token) {
        return (SchemaNode) new SchemaNode(token.getContent()).setPositionBegin(token.getPosition());
    }

    public static DALNode bracketSymbolNode(Token token, DALNode node) {
        return new SymbolNode(((ConstNode) node).getValue(), SymbolNode.Type.BRACKET)
                .setPositionBegin(token.getPosition());
    }

    public static DALNode parenthesesNode(Token token, DALNode node) {
        return new DALExpression(null, new DALOperator.Parentheses(), node).setPositionBegin(token.getPosition());
    }

    public static DALNode constString(Token token, DALNode node) {
        return ((NodeCollection) node).toConstString(token.getPosition());
    }

    public static DALNode regex(Token token, DALNode node) {
        return ((NodeCollection) node).teRegexNode(token.getPosition());
    }

    public static DALNode objectScopeNode(Token token, DALNode node) {
        return ((NodeCollection) node).objectScopeNode(token.getPosition());
    }

    public static DALNode constTrue(Token token) {
        return new ConstNode(true);
    }

    public static DALNode constFalse(Token token) {
        return new ConstNode(false);
    }

    public static DALNode constNull(Token token) {
        return new ConstNode(null);
    }

    public static WildcardNode wildcardNode(Token token) {
        return new WildcardNode(token.getContent());
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

    public boolean judge(DALNode actualNode, DALOperator.Equal operator, RuntimeContextBuilder.DALRuntimeContext context) {
        return assertEquals(evaluateData(context), evaluateAndWrapperFailureMessage(actualNode, context),
                getPositionBegin());
    }

    public boolean judge(DALNode actualNode, Matcher operator, RuntimeContextBuilder.DALRuntimeContext context) {
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
            throw new RuntimeException(format("Cannot compare between%sand%s", actual.inspect(), expected.inspect()),
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
