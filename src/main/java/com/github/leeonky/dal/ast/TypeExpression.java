package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.IllegalTypeException;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static java.lang.String.format;

public class TypeExpression extends Node {
    private final Node instance;
    private final List<SchemaNode> schemaNodes = new ArrayList<>();
    private final ObjectRef objectRef = new ObjectRef();
    private Operator operator = Operator.NONE;

    public TypeExpression(Node instance, SchemaNode node) {
        this.instance = instance;
        schemaNodes.add(node);
    }

    public Object getTypeInstance() {
        return objectRef.instance;
    }

    public String getSchemaName() {
        return schemaNodes.get(0).getSchema();
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        try {
            boolean failed = isFailed(context, objectRef);
            if (failed)
                System.err.println("Warning: Type assertion `" + inspect() + "` got false.");
            return !failed;
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
    public String inspect() {
        return format("%s is %s", instance.inspect(), schemaNodes.stream().map(SchemaNode::inspect)
                .collect(Collectors.joining(" " + operator.value + " ")));
    }

    public void appendSchema(Operator operator, SchemaNode schemaNode) {
        if (this.operator != Operator.NONE && this.operator != operator)
            throw new SyntaxException(schemaNode.getPositionBegin(), "Schema operator should be consistent");
        this.operator = operator;
        schemaNodes.add(schemaNode);
    }

    final public TypeWhichExpression which(Node whichClause) {
        return new TypeWhichExpression(this, whichClause);
    }

    public enum Operator {
        NONE(""), AND("|"), OR("/");

        private final String value;

        Operator(String value) {
            this.value = value;
        }
    }

    static class ObjectRef {
        public Object instance;
    }
}
