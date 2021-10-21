package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.IllegalTypeException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.compiler.Constants.SCHEMA_DELIMITER;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

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
            schemaNodes.stream().allMatch(schemaNode -> verifyAndConvertAsSchemaType(context, schemaNode, objectRef));
            return objectRef.instance;
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }

    //    TODO refactor
    private boolean verifyAndConvertAsSchemaType(RuntimeContextBuilder.RuntimeContext context,
                                                 SchemaNode schemaNode, ObjectRef objectRef) {
        if (elementSchema) {
            AtomicInteger index = new AtomicInteger(0);
            objectRef.instance = instance.evaluateDataObject(context).getListObjects().stream()
                    .map(dataObject -> {
                        int i = index.getAndIncrement();
                        try {
                            return schemaNode.getConstructorViaSchema(context).apply(dataObject);
                        } catch (IllegalTypeException exception) {
                            throw new AssertionFailure(exception.assertionFailureMessage(schemaNode, i),
                                    schemaNode.getPositionBegin());
                        }
                    })
                    .collect(Collectors.toList());
            return true;
        } else
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
        String schemaChain = schemaNodes.stream().map(SchemaNode::inspect)
                .collect(joining(format(" %s ", SCHEMA_DELIMITER)));
        if (elementSchema)
            return format("is [%s]", schemaChain);
        return format("is %s", schemaChain);
    }

    @Override
    public Object getRootName() {
        return instance.getRootName();
    }
}
