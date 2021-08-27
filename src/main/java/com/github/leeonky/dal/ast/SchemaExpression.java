package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.token.IllegalTypeException;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.Constants.SCHEMA_DELIMITER;
import static java.lang.String.format;

public class SchemaExpression extends Node {
    private final Node instance;
    private final List<SchemaNode> schemaNodes = new ArrayList<>();
    private final ObjectRef objectRef = new ObjectRef();

    public SchemaExpression(Node instance, SchemaNode node) {
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
            //TODO throw error
            return !failed;
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }

    private boolean isFailed(RuntimeContext context, ObjectRef objectRef) {
        return !schemaNodes.stream().allMatch(schemaNode -> verifyAndConvertAsSchemaType(context, schemaNode, objectRef));
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
                .collect(Collectors.joining(String.format(" %s ", SCHEMA_DELIMITER))));
    }

    public void appendSchema(SchemaNode schemaNode) {
        schemaNodes.add(schemaNode);
    }

    final public SchemaWhichExpression which(Node whichClause) {
        return new SchemaWhichExpression(this, whichClause);
    }

    static class ObjectRef {
        public Object instance;
    }
}
