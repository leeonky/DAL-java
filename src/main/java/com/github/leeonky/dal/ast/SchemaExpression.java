package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.IllegalTypeException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.compiler.Constants.SCHEMA_DELIMITER;
import static java.lang.String.format;
import static java.lang.String.join;
import static java.util.Collections.nCopies;
import static java.util.stream.Collectors.joining;

public class SchemaExpression extends DALNode {
    private final DALNode instance;
    private final List<SchemaNodeBak> schemaNodeBaks = new ArrayList<>();
    private final ObjectRef objectRef = new ObjectRef();
    private final int dimension;

    public SchemaExpression(DALNode previous, List<SchemaNodeBak> schemaNodeBaks, int dimension) {
        instance = previous;
        this.schemaNodeBaks.addAll(schemaNodeBaks);
        this.dimension = dimension;
    }

    public String getSchemaName() {
        return schemaNodeBaks.get(schemaNodeBaks.size() - 1).getSchema();
    }

    @Override
    public Object evaluate(RuntimeContextBuilder.DALRuntimeContext context) {
        try {
            schemaNodeBaks.forEach(schemaNode -> verifyAndConvertAsSchemaType(context, schemaNode, objectRef));
            return objectRef.instance;
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }

    @Override
    public Data evaluateData(RuntimeContextBuilder.DALRuntimeContext context) {
        return context.wrap(evaluate(context), getSchemaName(), dimension > 0);
    }

    private void verifyAndConvertAsSchemaType(RuntimeContextBuilder.DALRuntimeContext context,
                                              SchemaNodeBak schemaNodeBak, ObjectRef objectRef) {
        Data input = instance.evaluateData(context);
        if (dimension == 1) {
            if (!input.isList())
                throw new SyntaxException("Expecting a list but was" + input.inspect(), instance.getPositionBegin());
            AtomicInteger index = new AtomicInteger(0);
            objectRef.instance = input.getListObjects().stream().map(element -> convertViaSchema(context, schemaNodeBak,
                    element, format("%s[%d]", instance.inspect(), index.getAndIncrement())))
                    .collect(Collectors.toList());
        } else
            objectRef.instance = convertViaSchema(context, schemaNodeBak, input, instance.inspect());
    }

    private Object convertViaSchema(RuntimeContextBuilder.DALRuntimeContext context, SchemaNodeBak schemaNodeBak,
                                    Data element, String input) {
        try {
            return schemaNodeBak.getConstructorViaSchema(context).apply(element);
        } catch (IllegalTypeException exception) {
            throw new AssertionFailure(exception.assertionFailureMessage(input.isEmpty() ? input : input + " ",
                    schemaNodeBak), schemaNodeBak.getPositionBegin());
        }
    }

    @Override
    public String inspect() {
        return (instance instanceof InputNode ? "" : instance.inspect() + " ") + inspectClause();
    }

    private SchemaWhichExpression which(DALNode whichClause, boolean omitWhich) {
        return (SchemaWhichExpression) new SchemaWhichExpression(this, whichClause, omitWhich)
                .setPositionBegin(getPositionBegin());
    }

    public SchemaWhichExpression omitWhich(DALNode n) {
        return which(n, true);
    }

    public SchemaWhichExpression which(DALNode n) {
        return which(n, false);
    }

    static class ObjectRef {
        public Object instance;
    }

    @Override
    public String inspectClause() {
        return format("is %s%s%s", join("", nCopies(dimension, "[")),
                schemaNodeBaks.stream().map(SchemaNodeBak::inspect).collect(joining(format(" %s ", SCHEMA_DELIMITER))),
                join("", nCopies(dimension, "]")));
    }

    @Override
    public Object getRootName() {
        return instance.getRootName();
    }
}
