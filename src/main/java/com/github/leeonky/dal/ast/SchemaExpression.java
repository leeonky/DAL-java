package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.AssertionFailure;
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

    public SchemaExpression(Node previous, List<SchemaNode> schemaNodes) {
        instance = previous;
        this.schemaNodes.addAll(schemaNodes);
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
            return matches(context, objectRef);
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }

    private boolean matches(RuntimeContext context, ObjectRef objectRef) {
        return schemaNodes.stream().allMatch(schemaNode -> verifyAndConvertAsSchemaType(context, schemaNode, objectRef));
    }

    private boolean verifyAndConvertAsSchemaType(RuntimeContext context, SchemaNode schemaNode, ObjectRef objectRef) {
        try {
            objectRef.instance = schemaNode.getConstructorViaSchema(context).apply(instance.evaluateDataObject(context));
            return true;
        } catch (IllegalTypeException exception) {
            throw new AssertionFailure(exception.assertionFailureMessage(schemaNode), schemaNode.getPositionBegin());
        }
    }

    @Override
    public String inspect() {
        String instanceInspect = instance instanceof InputNode ? "" : instance.inspect() + " ";
        return format("%sis %s", instanceInspect, schemaNodes.stream().map(SchemaNode::inspect)
                .collect(Collectors.joining(format(" %s ", SCHEMA_DELIMITER))));
    }

    public void appendSchema(SchemaNode schemaNode) {
        schemaNodes.add(schemaNode);
    }

    public SchemaWhichExpression which(Node whichClause, boolean omitWhich) {
        return (SchemaWhichExpression) new SchemaWhichExpression(this, whichClause, omitWhich)
                .setPositionBegin(getPositionBegin());
    }

    public SchemaWhichExpression omitWhich(Node n) {
        return which(n, true);
    }

    public SchemaWhichExpression which(Node n) {
        return which(n, false);
    }

    static class ObjectRef {
        public Object instance;
    }
}
