package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.ConstructorViaSchema;
import com.github.leeonky.dal.runtime.RuntimeContext;

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
