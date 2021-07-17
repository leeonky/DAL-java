package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.token.IllegalTypeException;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class SchemaAssertionExpression extends Node {
    private final Node instance;
    private final List<SchemaNode> schemaNodes = new ArrayList<>();
    private final Node assertion;
    private Operator operator = Operator.NONE;

    public SchemaAssertionExpression(Node instance, SchemaNode schemaNode, Node assertion) {
        this.instance = instance;
        schemaNodes.add(schemaNode);
        this.assertion = assertion;
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        try {
            ObjectRef objectRef = new ObjectRef();
            if (isFailed(context, objectRef)) {
                System.err.println("Warning: Type assertion `" + inspect() + "` got false.");
                return false;
            }
            return context.wrapInputValueAndEvaluate(objectRef.instance, assertion);
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }

    private boolean isFailed(RuntimeContext context, ObjectRef objectRef) {
        return operator == Operator.AND ? !schemaNodes.stream().allMatch(schemaNode -> verifyAndConvertAsSchemaType(context, schemaNode, objectRef))
                : schemaNodes.stream().noneMatch(schemaNode -> verifyAndConvertAsSchemaType(context, schemaNode, objectRef));
    }

    private boolean verifyAndConvertAsSchemaType(RuntimeContext context, SchemaNode schemaNode, ObjectRef objectRef) {
        try {
            objectRef.instance = schemaNode.getConstructorViaSchema(context).apply(instance.evaluate(context), context);
            return true;
        } catch (IllegalTypeException ignore) {
            System.err.println("Warning: Type assertion `" + schemaNode.inspect() + "` got false.");
            return false;
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof SchemaAssertionExpression
                && Objects.equals(instance, ((SchemaAssertionExpression) obj).instance)
                && Objects.equals(schemaNodes, ((SchemaAssertionExpression) obj).schemaNodes)
                && Objects.equals(operator, ((SchemaAssertionExpression) obj).operator)
                && Objects.equals(assertion, ((SchemaAssertionExpression) obj).assertion);
    }

    @Override
    public String inspect() {
        //TODO inspect schema list
        return String.format("%s is %s which %s", instance.inspect(), schemaNodes.get(0).inspect(), assertion.inspect());
    }

    public void appendSchema(Operator operator, SchemaNode schemaNode) {
        this.operator = operator;
        schemaNodes.add(schemaNode);
        //TODO check operator
    }

    public enum Operator {
        NONE, AND, OR
    }

    class ObjectRef {
        public Object instance;
    }
}
