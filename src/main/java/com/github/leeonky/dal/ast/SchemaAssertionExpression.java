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
        Object value;
        try {
            if (operator == Operator.AND) {
                Object[] objects = schemaNodes.stream().map(schemaNode ->
                        verifyAndConvertAsSchemaType(context, schemaNode)).toArray();
                value = objects[objects.length - 1];
            } else {
                SchemaNode schemaNode = schemaNodes.get(0);
                value = verifyAndConvertAsSchemaType(context, schemaNode);
            }
            return context.wrapInputValueAndEvaluate(value, assertion);
        } catch (IllegalTypeException ignore) {
            System.err.println("Warning: Type assertion `" + inspect() + "` got false.");
            return false;
        }
    }

    private Object verifyAndConvertAsSchemaType(RuntimeContext context, SchemaNode schemaNode) {
        try {
            return schemaNode.getConstructorViaSchema(context).apply(instance.evaluate(context), context);
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), schemaNode.getPositionBegin());
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof SchemaAssertionExpression
                && Objects.equals(instance, ((SchemaAssertionExpression) obj).instance)
                && Objects.equals(schemaNodes, ((SchemaAssertionExpression) obj).schemaNodes)
                && Objects.equals(assertion, ((SchemaAssertionExpression) obj).assertion);
    }

    @Override
    public String inspect() {
        return String.format("%s is %s which %s", instance.inspect(), schemaNodes.get(0).inspect(), assertion.inspect());
    }

    public void appendSchema(Operator operator, SchemaNode schemaNode) {
        schemaNodes.add(schemaNode);
        //TODO check operator
    }

    public enum Operator {
        NONE, AND, OR
    }
}
