package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.IllegalTypeException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.compiler.Constants.SCHEMA_DELIMITER;
import static java.lang.String.format;

public class SchemaExpression extends Node {
    private final Node instance;
    private final List<SchemaNode> schemaNodes = new ArrayList<>();
    private final ObjectRef objectRef = new ObjectRef();
    private final boolean elementSchema;

    public SchemaExpression(Node previous, List<SchemaNode> schemaNodes, boolean elementSchema) {
        instance = previous;
        this.schemaNodes.addAll(schemaNodes);
        this.elementSchema = elementSchema;
    }

    public String getSchemaName() {
        return schemaNodes.get(0).getSchema();
    }

    @Override
    public Object evaluate(RuntimeContextBuilder.RuntimeContext context) {
        try {
            matches(context, objectRef);
            return objectRef.instance;
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }

    private boolean matches(RuntimeContextBuilder.RuntimeContext context, ObjectRef objectRef) {
        return schemaNodes.stream().allMatch(schemaNode -> verifyAndConvertAsSchemaType(context, schemaNode, objectRef));
    }

    private boolean verifyAndConvertAsSchemaType(RuntimeContextBuilder.RuntimeContext context, SchemaNode schemaNode, ObjectRef objectRef) {
        try {
            objectRef.instance = schemaNode.getConstructorViaSchema(context).apply(instance.evaluateDataObject(context));
            return true;
        } catch (IllegalTypeException exception) {
            throw new AssertionFailure(exception.assertionFailureMessage(schemaNode), schemaNode.getPositionBegin());
        }
    }

    @Override
    public String inspect() {
        return (instance instanceof InputNode ? "" : instance.inspect() + " ") + inspectClause();
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

    @Override
    public String inspectClause() {
        return format("is %s", schemaNodes.stream().map(SchemaNode::inspect)
                .collect(Collectors.joining(format(" %s ", SCHEMA_DELIMITER))));
    }

    @Override
    public Object getRootName() {
        return instance.getRootName();
    }
}
