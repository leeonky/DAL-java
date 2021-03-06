package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ConstructorViaSchema;
import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;

import java.util.Objects;

public class SchemaNode extends Node {
    private final String schema;

    public SchemaNode(String schema) {
        this.schema = schema;
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        throw new IllegalStateException();
    }

    public ConstructorViaSchema getConstructorViaSchema(RuntimeContext context) {
        return context.searchConstructor(schema).orElseThrow(() ->
                new RuntimeException("Schema '" + schema + "' not registered", getPositionBegin()));
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof SchemaNode && Objects.equals(schema, ((SchemaNode) obj).schema);
    }

    @Override
    public String inspect() {
        return schema;
    }
}
