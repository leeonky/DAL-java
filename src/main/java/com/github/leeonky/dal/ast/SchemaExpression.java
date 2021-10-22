package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.SyntaxException;
import com.github.leeonky.dal.runtime.DataObject;
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
            schemaNodes.forEach(schemaNode -> verifyAndConvertAsSchemaType(context, schemaNode, objectRef));
            return objectRef.instance;
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }

    private void verifyAndConvertAsSchemaType(RuntimeContextBuilder.RuntimeContext context,
                                              SchemaNode schemaNode, ObjectRef objectRef) {
        DataObject input = instance.evaluateDataObject(context);
        if (elementSchema) {
            if (!input.isList())
                throw new SyntaxException("Expecting a list but was" + input.inspect(), instance.getPositionBegin());
            AtomicInteger index = new AtomicInteger(0);
            objectRef.instance = input.getListObjects().stream().map(element -> convertViaSchema(context, schemaNode,
                    element, format("%s[%d] ", instance.inspect(), index.getAndIncrement())))
                    .collect(Collectors.toList());
        } else
            objectRef.instance = convertViaSchema(context, schemaNode, input, instance.inspect());
    }

    private Object convertViaSchema(RuntimeContextBuilder.RuntimeContext context, SchemaNode schemaNode,
                                    DataObject element, String input) {
        try {
            return schemaNode.getConstructorViaSchema(context).apply(element);
        } catch (IllegalTypeException exception) {
            throw new AssertionFailure(exception.assertionFailureMessage(input, schemaNode),
                    schemaNode.getPositionBegin());
        }
    }

    @Override
    public String inspect() {
        return (instance instanceof InputNode ? "" : instance.inspect() + " ") + inspectClause();
    }

    private SchemaWhichExpression which(Node whichClause, boolean omitWhich) {
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
