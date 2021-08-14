package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ConstructorViaSchema;
import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;

public class SchemaNode extends Node {
    private final String schema;

    public SchemaNode(String schema) {
        this.schema = schema;
    }

    public ConstructorViaSchema getConstructorViaSchema(RuntimeContext context) {
        return context.searchConstructor(schema).orElseThrow(() ->
                new RuntimeException("Schema '" + schema + "' not registered", getPositionBegin()));
    }

    @Override
    public String inspect() {
        return schema;
    }

    public String getSchema() {
        return schema;
    }
}
