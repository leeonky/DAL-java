package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.IllegalTypeException;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

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
            return context.wrapInputValueAndEvaluate(objectRef.instance, assertion, schemaNodes.get(0));
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }

    private boolean isFailed(RuntimeContext context, ObjectRef objectRef) {
        return operator == Operator.AND ?
                !schemaNodes.stream().allMatch(schemaNode -> verifyAndConvertAsSchemaType(context, schemaNode, objectRef))
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
        String assertionClause = assertion.inspect();
        String whichClause = "true".equals(assertionClause) ? "" : String.format(" which %s", assertionClause);
        return String.format("%s is %s%s", instance.inspect(), schemaNodes.stream().map(SchemaNode::inspect)
                .collect(Collectors.joining(" " + operator.value + " ")), whichClause);
    }

    public void appendSchema(Operator operator, SchemaNode schemaNode) {
        if (this.operator != Operator.NONE && this.operator != operator)
            throw new SyntaxException(schemaNode.getPositionBegin(), "Schema operator should be consistent");
        this.operator = operator;
        schemaNodes.add(schemaNode);
    }

    public enum Operator {
        NONE(""), AND("|"), OR("/");

        private final String value;

        Operator(String value) {
            this.value = value;
        }
    }

    class ObjectRef {
        public Object instance;
    }
}
