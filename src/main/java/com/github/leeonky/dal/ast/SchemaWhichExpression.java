package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;

public class SchemaWhichExpression extends Node {
    private final SchemaExpression schemaExpression;
    private final Node clause;

    public SchemaWhichExpression(SchemaExpression schemaExpression, Node clause) {
        this.schemaExpression = schemaExpression;
        this.clause = clause;
    }

    @Override
    public String inspect() {
        return schemaExpression.inspect() + " which " + clause.inspect();
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        try {
            if ((Boolean) schemaExpression.evaluate(context))
                return context.wrapInputValueAndEvaluate(schemaExpression.getTypeInstance(), clause, schemaExpression.getSchemaName());
            return false;
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }
}
